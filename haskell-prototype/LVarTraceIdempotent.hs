{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns, BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | This (experimental) module generalizes the Par monad to allow
-- arbitrary LVars (lattice variables), not just IVars.

module LVarTraceIdempotent 
  (LVar(), newLV, getLV, putLV, freezeLV, 
   newPool, addHandler, quiesce, fork, liftIO, yield, Par(),
   runParIO
  ) where

import           Control.Monad hiding (sequence, join)
import           Control.Concurrent hiding (yield)
import           Control.DeepSeq
import           Control.Applicative
import           Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Concurrent.Counter as C
import qualified Data.Concurrent.Bag as B
import           GHC.Conc hiding (yield)
import           System.IO.Unsafe (unsafePerformIO)
import           Prelude  hiding (mapM, sequence, head, tail)

import           Common (forkWithExceptions)

import qualified Sched as Sched
  
------------------------------------------------------------------------------
-- LVar and Par monad representation:
------------------------------------------------------------------------------

-- AJT TODO: add commentary
data LVar a d = LVar {
  state  :: a,                 -- the current, "global" state of the LVar
  status :: IORef (Status d)   -- is the LVar active or frozen?
}

data Status d 
  = Frozen                     -- further changes to the state are forbidden
  | Active (B.Bag (Listener d))  -- bag of blocked threshold reads and handlers
    
data Listener d = Listener {
  onUpdate :: d -> B.Token (Listener d) -> Sched.Queue Trace -> IO (),
  onFreeze ::      B.Token (Listener d) -> Sched.Queue Trace -> IO ()
}

data HandlerPool = HandlerPool {
  numHandlers      :: C.Counter, 
  blockedOnQuiesce :: B.Bag Trace
}

newtype Trace = Trace {
  exec :: Sched.Queue Trace -> IO ()
}

newtype Par a = Par {
  trace :: (a -> Trace) -> Trace
}

instance Functor Par where
  fmap f m = Par $ \k -> trace m (k . f)

instance Monad Par where
  return a = Par $ \k -> k a
  m >>= c  = Par $ \k -> trace m $ \a -> trace (c a) k

instance Applicative Par where
  (<*>) = ap
  pure  = return
  
------------------------------------------------------------------------------
-- A few auxiliary functions
------------------------------------------------------------------------------  

mkPar :: ((a -> Trace) -> Sched.Queue Trace -> IO ()) -> Par a
mkPar f = Par $ \k -> Trace $ \q -> f k q

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust Nothing  _ = return ()
whenJust (Just a) f = f a

isFrozen :: LVar a d -> IO Bool
isFrozen (LVar _ status) = do
  curStatus <- readIORef status
  case curStatus of
    Active _ -> return False
    Frozen   -> return True
    
------------------------------------------------------------------------------
-- LVar operations:
------------------------------------------------------------------------------
    
-- | Create an LVar
newLV :: IO a -> Par (LVar a d)
newLV init = mkPar $ \k q -> do
  state     <- init
  listeners <- B.new
  status    <- newIORef $ Active listeners
  exec (k $ LVar state status) q

-- | Do a threshold read on an LVar
getLV :: (LVar a d)                  -- ^ the LVar 
      -> (a -> Bool -> IO (Maybe b)) -- ^ already past threshold?
      -> (d ->         IO (Maybe b)) -- ^ does d pass the threshold?
      -> Par b
getLV lv@(LVar state status) globalThresh deltaThresh = mkPar $ \k q ->
  let unblockWhen thresh tok q = do
        tripped <- thresh
        whenJust tripped $ \b -> do
          B.remove tok 
          Sched.pushWork q (k b)
      onUpdate d = unblockWhen $ deltaThresh d
      onFreeze   = unblockWhen $ globalThresh state True
  in do

    -- tradeoff: we fastpath the case where the LVar is already beyond the
    -- threshhold by polling *before* enrolling the callback.  The price is
    -- that, if we are not currently above the threshhold, we will have to poll
    -- *again* after enrolling the callback.  This race may also result in the
    -- continuation being executed twice, which is permitted by idempotence.

    curStatus <- readIORef status
    case curStatus of
      Frozen -> do 
        tripped <- globalThresh state True
        case tripped of
          Just b -> exec (k b) q -- already past the threshold; invoke the
                                 -- continuation immediately                    
          Nothing -> sched q     
      Active listeners -> do
        tripped <- globalThresh state False
        case tripped of
          Just b -> exec (k b) q -- already past the threshold; invoke the
                                 -- continuation immediately        
          
          Nothing -> do          -- *transiently* not past the threshhold; block        
            -- add listener, i.e., move the continuation to the waiting bag
            tok <- B.put listeners $ Listener onUpdate onFreeze

            -- but there's a race: the threshold might be passed (or the LVar
            -- frozen) between our check and the enrollment as a listener, so we
            -- must poll again
            frozen <- isFrozen lv
            tripped' <- globalThresh state frozen
            case tripped' of
              Just b -> do
                B.remove tok  -- remove the listener we just added, and
                exec (k b) q  -- execute the continuation. this work might be
                              -- redundant, but by idempotence that's OK
              Nothing -> sched q

-- | Update an LVar
putLV :: LVar a d            -- ^ the LVar
      -> (a -> IO (Maybe d)) -- ^ how to do the put, and whether the LVar's
                             -- value changed
      -> Par ()
putLV (LVar state status) doPut = mkPar $ \k q -> do
  delta <- doPut state
  whenJust delta $ \d -> do
    curStatus <- readIORef status
    case curStatus of
      Frozen -> error "Attempt to change a frozen LVar"
      Active listeners -> 
        let invoke (Listener onUpdate _) tok = onUpdate d tok q
        in B.foreach listeners invoke
  exec (k ()) q

-- | Freeze an LVar (limited nondeterminism)
freezeLV :: LVar a d -> Par ()
freezeLV (LVar _ status) = mkPar $ \k q -> do
  oldStatus <- atomicModifyIORef status $ \s -> (Frozen, s)    
  case oldStatus of
    Frozen -> return ()
    Active listeners -> 
      let invoke (Listener _ onFreeze) tok = onFreeze tok q
      in B.foreach listeners invoke
  exec (k ()) q
  
------------------------------------------------------------------------------
-- Handler pool operations:
------------------------------------------------------------------------------  

-- | Create a handler pool
newPool :: Par HandlerPool
newPool = mkPar $ \k q -> do
  cnt <- C.new
  bag <- B.new
  exec (k $ HandlerPool cnt bag) q

-- | Add a handler to an existing pool
addHandler :: HandlerPool                 -- ^ pool to enroll in 
           -> LVar a d                    -- ^ LVar to listen to
           -> (a -> IO (Maybe (Par ())))  -- ^ initial callback
           -> (d -> IO (Maybe (Par ())))  -- ^ subsequent callbacks: updates
           -> Par ()
addHandler hp (LVar state status) globalThresh updateThresh = 
  let cnt = numHandlers hp    
      
      spawnWhen thresh q = do
        tripped <- thresh
        whenJust tripped $ \cb -> do
          C.inc cnt   -- record callback invocation in pool        
          
          -- create callback thread, which is responsible for recording its
          -- termination in the handler pool
          Sched.pushWork q $ trace cb onFinishCB
          
      -- what to do when a callback thread completes
      onFinishCB _ = Trace $ \q -> do
        C.dec cnt                 -- record callback completion in pool
        quiescent <- C.poll cnt   -- check for (transient) quiescence
        when quiescent $          -- wake any threads waiting on quiescence
          let invoke t tok = do
                B.remove tok
                Sched.pushWork q t                
          in B.foreach (blockedOnQuiesce hp) invoke
        sched q
        
      onUpdate d _ q = spawnWhen (updateThresh d) q
      onFreeze   _ _ = return ()
        
  in mkPar $ \k q -> do
    curStatus <- readIORef status 
    case curStatus of
      Active listeners ->             -- enroll the handler as a listener
        do B.put listeners $ Listener onUpdate onFreeze; return ()
      Frozen -> return ()             -- frozen, so no need to enroll 
    spawnWhen (globalThresh state) q  -- poll globally to see whether we should
                                      -- launch any callbacks now
    exec (k ()) q 

-- | Block until a handler pool is quiescent      
quiesce :: HandlerPool -> Par ()
quiesce (HandlerPool cnt bag) = mkPar $ \k q -> do
  -- tradeoff: we assume that the pool is not yet quiescent, and thus enroll as
  -- a blocked thread prior to checking for quiescence
  tok <- B.put bag (k ())
  quiescent <- C.poll cnt
  if quiescent then do
    B.remove tok
    exec (k ()) q 
  else sched q

------------------------------------------------------------------------------
-- Par monad operations:
------------------------------------------------------------------------------

-- | Fork a child thread
fork :: Par () -> Par ()
fork child = mkPar $ \k q -> do
  Sched.pushWork q (k ()) -- "Work-first" policy.
  exec (trace child $ const (Trace sched)) q
  -- Sched.pushWork q (trace child emptyCont) -- "Help-first" policy.  Generally bad.
  --   exec (k ()) q

-- | Perform an IO action
liftIO :: IO a -> Par a
liftIO io = mkPar $ \k q -> do
  r <- io
  exec (k r) q

-- | Cooperatively schedule other threads
yield :: Par ()  
yield = mkPar $ \k q -> do
  Sched.yieldWork q (k ())
  sched q
  
{-# INLINE sched #-}
sched :: Sched.Queue Trace -> IO ()
sched q = do
  n <- Sched.next q
  case n of
    Just t  -> exec t q
    Nothing -> return ()

-- Forcing evaluation of a LVar is fruitless.
instance NFData (LVar a d) where
  rnf _ = ()
  
{-# INLINE runPar_internal #-}
runPar_internal :: Par a -> IO a
runPar_internal c = do
  queues <- Sched.new numCapabilities 
  
  -- We create a thread on each CPU with forkOn.  The CPU on which
  -- the current thread is running will host the main thread; the
  -- other CPUs will host worker threads.
  main_cpu <- Sched.currentCPU
  m <- newEmptyMVar  
  forM_ (zip [0..] queues) $ \(cpu, q) ->
    forkWithExceptions (forkOn cpu) "worker thread" $ 
      if cpu == main_cpu 
        then let k x = Trace $ \q -> do 
                   sched q      -- ensure any remaining, enabled threads run to 
                   putMVar m x  -- completion prior to returning the result
             in exec (trace c k) q
        else sched q
  takeMVar m  

runPar :: Par a -> a
runPar = unsafePerformIO . runPar_internal

-- | A version that avoids an internal `unsafePerformIO` for calling
-- contexts that are already in the `IO` monad.
runParIO :: Par a -> IO a
runParIO = runPar_internal