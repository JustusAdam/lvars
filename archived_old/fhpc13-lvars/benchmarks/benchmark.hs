
-- | This is an HSBencher script.

module Main where

import qualified Data.Set as Set

import GHC.Conc           (getNumProcessors)
import System.Environment (getEnvironment)
import System.IO.Unsafe   (unsafePerformIO)

import HSBencher.Types(BenchSpace(..), Benchmark(..), ParamSetting(..), DefaultParamMeaning(..)
                       -- compileOptsOnly, enumerateBenchSpace, toCompileFlags,
                       -- makeBuildID, BuildID, 
                      )
--import HSBencher.App (defaultMainWithBechmarks)
import HSBencher
import Control.Monad
import System.FilePath
import System.Process
import Control.Monad.IO.Class
-- Temp:
-- import Text.PrettyPrint.GenericPretty (Out(doc,docPrec), Generic)


main :: IO ()
main = do
  putStrLn$ "Exploring thread settings: " ++ show threadSelection
  defaultMainModifyConfig (addBenchmarks bls . \c -> c { buildMethods = [justRun] })

--------------------------------------------------------------------------------
-- Here are the actual benchmarks:
--------------------------------------------------------------------------------

allSettings = varyThreads defaultSettings

makeBenchmarks name = map (mkBench . show) [1,2,4,8,16,31,64]
   where
     mkBench p = mkBenchmark name ["none","/tmp/rand_320000_40000","10",p] space
     space = And [allSettings, Set NoMeaning (CompileParam name)]

justRun :: BuildMethod
justRun = BuildMethod
  { methodName = "run"
  , canBuild = AnyFile
  , concurrentBuild = False
  , compile = \_conf _id flags _env path -> liftIO $ StandAloneBinary <$> readCreateProcess (proc "which" [takeBaseName path]) ""x
  , clean = \_ _ _ -> pure ()
  , setThreads = Nothing
  }
  

bls :: [Benchmark DefaultParamMeaning]
bls = join

  -- # Arguments:
  -- #  - filename of graph
  -- #  - number of hops to take from node 0
  -- #  - microseconds of work to be done by function applied to each node

  ------------------------------------------------------------  
 -- Desktop configuration:
 ------------------------------------------------------------  
 [ makeBenchmarks "bf-traverse-Strategies"
 , makeBenchmarks "bf-traverse-monad-par"
 , makeBenchmarks "bf-traverse-LVar"
 , makeBenchmarks "bf-traverse-ohua"
 ]

--------------------------------------------------------------------------------
-- Set up some common benchmark config spaces:
--------------------------------------------------------------------------------

-- Add the default Haskell compiler settings that we want:
defaultSettings :: BenchSpace DefaultParamMeaning
defaultSettings =
  And [ Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS")
      ]

-- TODO: make this an option:
threadSelection :: [Int]
threadSelection = unsafePerformIO $ do
  env <- getEnvironment
  p   <- getNumProcessors
  case lookup "THREADS" env of
    Just ls -> return$ map read $ words ls
    -- Arbitrary default policy 
    Nothing
      | p <= 16   -> return  [1 .. p]
      | otherwise -> return$ 1 : [2,4 .. p]

-- | Add variation from thread count.    
varyThreads :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
varyThreads conf =
  -- Or [ conf {- Unthreaded mode -}, threaded ]
  threaded
  where
    threaded = And [ Set NoMeaning (CompileParam "--ghc-options='-threaded'")
                   , Or (map fn threadSelection)
                   , conf ]
    fn n = Set (Threads n) $ RuntimeParam  ("+RTS -N"++ show n++" -RTS")

