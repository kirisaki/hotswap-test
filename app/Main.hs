{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Concurrent.ReadWriteLock as L
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Concurrent
import System.Environment

import GHCi.ObjLink
import Foreign

import Types

data UpdatableSO a = UpdatableSO
  { swapSO :: FilePath -> IO ()
  , withSO :: forall b . (a -> IO b) -> IO b
  }

data SOState a = SOState
  { lock :: L.RWLock
  , path :: FilePath
  , val :: a
  }

registerHotswap :: NFData a => String -> FilePath -> IO (UpdatableSO a)
registerHotswap symbolName firstPath = do
  firstVal <- force <$> loadNewSO symbolName firstPath
  firstLock <- L.new
  sMVar <- newMVar SOState
    { lock = firstLock
    , path = firstPath
    , val = firstVal
    }
  return UpdatableSO
    { swapSO = updateState sMVar symbolName
    , withSO = unWrap sMVar
    }

unWrap :: MVar (SOState a) -> (a -> IO b) -> IO b
unWrap mvar action = do
  SOState{..} <- readMVar mvar
  L.withRead lock $ action val

updateState :: NFData a => MVar (SOState a) -> String -> FilePath -> IO ()
updateState mvar symbolName nextPath = do
  newVal <- force <$> loadNewSO symbolName nextPath
  newLock <- L.new
  let
    newState = SOState
      { lock = newLock
      , path = nextPath
      , val = newVal
      }
  oldState <- swapMVar mvar newState
  L.withWrite (lock oldState) $
    unloadObj (path oldState)

foreign import ccall "dynamic"
  callExport :: FunPtr (IO (StablePtr a)) -> IO (StablePtr a)

loadNewSO :: String -> FilePath -> IO a
loadNewSO symName newSO = do
  -- initObjLinker is idempotent
  initObjLinker DontRetainCAFs

  loadObj newSO
  resolved <- resolveObjs
  unless resolved $ do
    unloadObj newSO
    throwIO (ErrorCall $ "Unable to resolve objects for " ++ newSO)
  c_sym <- lookupSymbol symName
  h <- case c_sym of
    Nothing -> do
      unloadObj newSO
      throwIO (ErrorCall "Could not find symbol")
    Just p_sym ->
      bracket (callExport $ castPtrToFunPtr p_sym) freeStablePtr deRefStablePtr
  purgeObj newSO
  return h


looper :: UpdatableSO SOHandles -> IO ()
looper so = do
  threadDelay 1000000
  withSO so $ \SOHandles{..} -> do
    putStrLn $ "someData = " ++ show someData
    someFn 7

main :: IO ()
main = do
  args <- getArgs
  so_path <- case args of
    [p] -> return p
    _ -> throwIO (ErrorCall "must give filepath of first .so as an arg")

  so <- registerHotswap "hs_soHandles" so_path

  bracket (forkIO (forever $ looper so)) killThread $ \_ -> forever $ do
    putStrLn "Next SO to use: "
    nextSO <- getLine
    swapSO so nextSO
