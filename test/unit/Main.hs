module Main where

import Test.Hspec.Runner
import TestUtils
import qualified Spec
-- import Test.Hspec.Formatters.Jenkins (xmlFormatter)
import           Test.Hspec
import           Control.Exception
import           Data.Time.Clock
import      Control.Concurrent
import           System.Process

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Unit-test:"
  getNumCapabilities >>= print
  setupStackFiles
  
  let config = defaultConfig { configPrintCpuTime = True }
                            --  , configFormatter = Just xmlFormatter
                            --  , configOutputFile = Right "test-logs/unit-test.xml"
                            --  }
  withFileLogging "main.log" $ hspecWith config $ around_ timeIt $ do
    describe "inner" $ it "inner" $ system "stack exec unit-test-inner" >> return ()
    Spec.spec
  where timeIt f = bracket getCurrentTime
                           (\start -> do end <- getCurrentTime
                                         putStrLn $ "\ttest-duration: " ++ show (diffUTCTime end start))
                           (const f)

-- main :: IO ()
-- main = do
--   summary <- withFile "results.xml" WriteMode $ \h -> do
--     let c = defaultConfig
--           { configFormatter = xmlFormatter
--           , configHandle = h
--           }
--     hspecWith c Spec.spec
--   unless (summaryFailures summary == 0) $
--     exitFailure

-- ---------------------------------------------------------------------
