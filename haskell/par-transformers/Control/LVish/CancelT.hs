{-# LANGUAGE Unsafe #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, InstanceSigs #-}

-- | A module for adding the cancellation capability.
-- 
--   In its raw form, this is unsafe, because cancelating could cancel something that
--   would have performed a visible side effect.

module Control.LVish.CancelT
       {-(
         -- * The transformer that adds the cancellation capability
         CancelT(), ThreadId,
         
         -- * Operations specific to CancelT
         runCancelT,

         -- * Forking computations that may be canceled
         forkCancelable, forkCancelableND,

         -- * More detailed interface, separating forking and TID creation
         createTid, forkCancelableWithTid, forkCancelableNDWithTid,

         -- * Performing cancellation
         cancel,         
         pollForCancel,
         cancelMe,

         -- * Boolean operations with cancellation
         asyncAnd
       )-}
       where

import Control.Monad.State as S
import Data.IORef
-- import Data.LVar.IVar

import Control.Par.Class as PC
import Control.Par.EffectSigs as E
import Control.Par.Class.Unsafe (ParMonad(..))

import qualified Data.Atomics.Counter as C
--------------------------------------------------------------------------------

-- | A Par-monad scheduler transformer that adds the cancellation capability.  To do
-- this, it must track, and periodically poll, extra mutable state for each
-- cancellable computation in the fork-tree.
newtype CancelT m a = CancelT ((StateT CState m) a)
  deriving (Monad, Functor)

unCancelT :: CancelT t t1 -> StateT CState t t1
unCancelT (CancelT m) = m

-- | Each computation has a boolean flag that stays True while it is still live.
--   Also, the state for one computation is linked to the state of children, so that
--   cancellation may be propagated transitively.
newtype CState = CState (IORef CPair)

instance MonadTrans CancelT where
  lift m = CancelT (lift m)

data CPair = CPair !Bool ![CState]

-- | Futures that may be canceled before the result is available.
-- newtype CFut a = CFut (IVar (Maybe a))
data CFut a = forall m . ParIVar m => CFut (Future m CFutFate) (Future m a)
-- Nothing signifies that it was canceled, whereas Just that it completed.

-- | In a deterministic scenario, a `CFut` can only be canceled or read, not both.
data CFutFate = Canceled | Read 
  deriving (Show,Read,Eq)


--------------------------------------------------------------------------------

-- | Run a Par monad with the cancellation effect.  Within this computation, it is
-- possible to cancel subtrees of computations.
runCancelT :: (PC.ParMonad m) => CancelT m a -> m a
runCancelT (CancelT st) = do
  ref <- internalLiftIO $ newIORef (CPair True []) 
  evalStateT st (CState ref) 



-- Check for cancellation of our thread.
poll :: (PC.ParMonad m, LVarSched m) => CancelT m Bool
poll = CancelT$ do
  CState ref  <- S.get
  CPair flg _ <- lift$ internalLiftIO$ readIORef ref
  return flg

-- | Check with the scheduler to see if the current thread has been canceled, if so
-- stop computing immediately.
pollForCancel :: (PC.ParMonad m, LVarSched m) => CancelT m ()
pollForCancel = do
  b <- poll
  unless b $ cancelMe

-- | Self cancellation.  Not the same as the parent calling `cancel`, because
-- cancelMe only cancels the current leaf of the computation tree, not its siblings
-- (not the entire subtree under a forkCancelable, that is).
cancelMe :: (LVarSched m) => CancelT m ()
cancelMe = lift $ returnToSched

-- | The type of cancellable thread identifiers.
type ThreadId = CState

-- | Fork a computation while retaining a handle on it that can be used to cancel it
-- (and all its descendents).  This is equivalent to `createTid` followed by `forkCancelableWithTid`.
-- 
-- This version is expected to retain /determinism/.  Therefore, the canceled
-- computations must be read only.
--
-- Finally, note that the return value 
forkCancelable :: (PC.ParIVar m, LVarSched m, ReadOnlyM m) => 
                  CancelT m a -> CancelT m (ThreadId, CFut a)
forkCancelable act = do
--    b <- poll   -- Tradeoff: we could poll once before the atomic op.
--    when b $ do
    tid <- createTid
    fut <- forkCancelableWithTid tid act
    return $! (tid,fut)

-- | This is a version of `forkCancelable` which allows side-effecting computation to
--   be canceled.  Be warned, that is a dangerous business.  Accordingly, this function
--   introduces nondeterminism and must register the "IO".
forkCancelableND :: (PC.ParIVar m, LVarSched m, HasIOM m) => 
                  CancelT m a -> CancelT m (ThreadId, CFut a)
-- TODO/FIXME: Should we allow the child computation to not have IO set if it likes?
forkCancelableND act = do
    tid <- createTid
    fut <- forkCancelableNDWithTid tid act
    return $! (tid, fut)


-- | Sometimes it is necessary to have a TID in scope *before* forking the
-- computations in question.  For that purpose, we provide a two-phase interface:
-- `createTid` followed by `forkCancelableWithTid`.
createTid :: (PC.ParMonad m, LVarSched m) => CancelT m ThreadId
{-# INLINE createTid #-}
createTid = CancelT$ do
    newSt <- lift$ internalLiftIO$ newIORef (CPair True [])
    return (CState newSt)

-- | Add a new computation to an existing cancelable thread.
-- 
--   Forking multiple computations with the same Tid are permitted; all threads will
--   be canceled as a group.
forkCancelableWithTid :: (PC.ParIVar m, LVarSched m, ReadOnlyM m) => 
                         ThreadId -> CancelT m a -> CancelT m (CFut a)
{-# INLINE forkCancelableWithTid #-}
forkCancelableWithTid tid act = forkInternal tid act

-- | Variant of `forkCancelableWithTid` that works for nondeterministic computations.
forkCancelableNDWithTid :: (PC.ParIVar m, LVarSched m, HasIOM m) => 
                         ThreadId -> CancelT m a -> CancelT m (CFut a)
{-# INLINE forkCancelableNDWithTid #-}
forkCancelableNDWithTid tid act = forkInternal tid act


-- Internal version -- no rules!
forkInternal :: forall m a . 
                 (PC.ParIVar m, LVarSched m) => 
                 ThreadId -> CancelT m a -> CancelT m (CFut a)
{-# INLINE forkInternal #-} 
forkInternal (CState childRef) (CancelT act) = CancelT$ do
    fate   <- lift (PC.new :: m CFutFate)
    result <- lift (PC.new :: m a)

    CState parentRef <- S.get
    -- Create new child state:  
    live <- lift$ internalLiftIO $ 
      atomicModifyIORef' parentRef $ \ orig@(CPair bl ls) ->
        if bl then
          -- Extend the tree by pointing to our child:
          (CPair True (CState childRef : ls), True)
        else -- The current thread has already been canceled: DONT fork:
          (orig, False)
    let act' = do x <- act 
                  lift $ PC.put_ result x
                  return ()
    if live then
       lift $ forkLV (evalStateT act' (CState childRef))
      else cancelMe'
    return $! CFut fate result



-- | Issue a cancellation request for a given sub-computation.  It will not be
-- fulfilled immediately, because the cancellation process is cooperative and only
-- happens when the thread(s) check in with the scheduler.
cancel :: (PC.ParMonad m, Monad m, HasPut (GetEffects m)) => ThreadId -> CancelT m ()
cancel = internal_cancel

internal_cancel :: (PC.ParMonad m, Monad m) => ThreadId -> CancelT m ()
internal_cancel (CState ref) = do
  -- To cancel a tree of threads, we atomically mark the root as canceled and then
  -- start chasing the children.  After we cancel any node, no further children may
  -- be added to that node.
  chldrn <- internalLiftIO$ atomicModifyIORef' ref $ \ orig@(CPair flg ls) -> 
    if flg 
     then (CPair False [], ls)
     else (orig, [])
  -- We could do this traversal in parallel if we liked...
  S.forM_ chldrn internal_cancel

cancelMe' :: LVarSched m => StateT CState m ()
cancelMe' = unCancelT cancelMe
  
instance (PC.ParSealed m) => PC.ParSealed (CancelT m) where
  type GetSession (CancelT m) = PC.GetSession m
  
instance (PC.ParMonad m, PC.ParIVar m, PC.LVarSched m) =>
         PC.LVarSched (CancelT m) where
  type LVar (CancelT m) = LVar m 

-- FIXME: we shouldn't need to fork a CANCELABLE thread here, should we?
--  forkLV act = do _ <- forkCancelable act; return ()
  forkLV act = PC.fork act
    
  newLV act = lift$ newLV act
  
  stateLV lvar =
    let (_::Proxy (m ()), a) = stateLV lvar
    in (Proxy::Proxy((CancelT m) ()), a)

  putLV lv putter = do
    pollForCancel
    lift $ putLV lv putter    

  getLV lv globThresh deltThresh = do
     pollForCancel
     x <- lift $ getLV lv globThresh deltThresh
    -- FIXME: repoll after blocking ONLY:
     pollForCancel
     return x
     
  returnToSched = lift returnToSched

instance (PC.ParQuasi m qm) => PC.ParQuasi (CancelT m) (CancelT qm) where
  toQPar :: (CancelT m) a -> (CancelT qm) a 
  toQPar (CancelT (S.StateT{runStateT})) =
    CancelT $ S.StateT $ toQPar . runStateT

instance (Functor qm, Monad qm, PC.ParMonad m, PC.ParIVar m, 
          LVarSched m, LVarSchedQ m qm, PC.ParQuasi (CancelT m) (CancelT qm) ) =>
         PC.LVarSchedQ (CancelT m) (CancelT qm) where

  freezeLV :: forall a d . LVar (CancelT m) a d -> (Proxy ((CancelT m) ()), (CancelT qm) ())
  freezeLV lvar = (Proxy, (do
    let lvar2 :: LVar m a d
        lvar2 = lvar -- This works because of the specific def for "type LVar" in the instance above...
    toQPar (pollForCancel :: CancelT m ())
    let frz :: LVar m a d -> (Proxy (m()), qm ())
        frz x = freezeLV x 
    CancelT (lift (snd (frz lvar2)))
    return ()))

instance PC.ParMonad m => PC.ParMonad (CancelT m) where
  fork (CancelT task) = CancelT $ do
    s0 <- S.get
    lift $ PC.fork $ do      
      (res,_) <- S.runStateT task s0
      return res
  internalLiftIO m = CancelT (lift (internalLiftIO m))

instance PC.ParFuture m => PC.ParFuture (CancelT m) where
  type Future (CancelT m) = Future m
  type FutContents (CancelT m) a = PC.FutContents m a
  spawn_ (CancelT task) = CancelT $ do
     s0 <- S.get
     lift $ PC.spawn_ $ do
           -- This spawned computation is part of the same tree for purposes of
           -- cancellation:
           (res,_) <- S.runStateT task s0
           return res     
  get iv = CancelT $ lift $ PC.get iv

instance PC.ParIVar m => PC.ParIVar (CancelT m) where
  new       = CancelT$ lift PC.new
  put_ iv v = CancelT$ lift$ PC.put_ iv v

--------------------------------------------------------------------------------

-- | A parallel and operation that not only signals its output early when it receives
-- a False input, but also attempts to cancel the other, unneeded computation.
--
-- FIXME: This is unsafe until there is a way to guarantee that read-only
-- computations are performed!
asyncAnd :: forall p . (PC.ParIVar p, LVarSched p, ReadOnlyM p)
            => ((CancelT p) Bool) -> (CancelT p Bool) -> (Bool -> CancelT p ()) -> CancelT p ()
-- Similar to `Control.LVish.Logical.asyncAnd`            
asyncAnd leftM rightM kont = do
  -- Atomic counter, if we are the second True we write the result:
  cnt <- internalLiftIO$ C.newCounter 0 -- TODO we could share this for 3+-way and.
  let launch mine theirs m =
             forkCancelableWithTid mine $
             -- Here are the possible states:
             -- T?   -- 1
             -- TT   -- 2
             -- F?   -- 100
             -- FT   -- 101
             -- TF   -- 101
             -- FF   -- 200
                   do b <- m
                      case b of
                        True  -> do n <- internalLiftIO$ C.incrCounter 1 cnt
                                    if n==2
                                      then kont True
                                      else return ()
                        False -> -- We COULD assume idempotency and execute kont False twice,
                                 -- but since we have the counter anyway let us dedup:
                                 do n <- internalLiftIO$ C.incrCounter 100 cnt                                    
                                    case n of
                                      100 -> do internal_cancel theirs; kont False
                                      101 -> kont False
                                      200 -> return ()
  tid1 <- createTid
  tid2 <- createTid  
  launch tid1 tid2 leftM
  launch tid2 tid1 rightM

  return ()


