module Main where

import Data.Time
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types hiding (error)
import TestUtils
import Control.Monad.IO.Class
import Control.Monad

main :: IO ()
main = do
  setupStackFiles
  symbolsAndRename

symbolsAndRename :: IO ()
symbolsAndRename = do
  times <- runSessionWithConfig config hieCommand fullCaps "bench/sample" $ do
    doc <- openDoc "Main.hs" "haskell"
    forM ([1..10] :: [Int]) $ \i -> do
      startT <- liftIO getCurrentTime
      syms <- getDocumentSymbols doc
      case syms of
        Left [] -> error "Empty"
        Right [] -> error "Empty"
        _ -> return ()
      rename doc (Position 32 12) ("displayItems" ++ show i)
      endT <- liftIO getCurrentTime
      -- end <- liftIO getTime_resolution
      return (diffUTCTime endT startT)
  print times
  where config = defaultConfig { logMessages = False }