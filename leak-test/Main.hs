{-# LANGUAGE OverloadedStrings #-}
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Control.Monad.IO.Class
import Control.Applicative.Combinators
import Control.Concurrent
import Control.Monad

main = runSessionWithConfig (defaultConfig { logStdErr = True, logMessages = True, messageTimeout = 500 }) "/Users/luke/Source/haskell-ide-engine/dist-newstyle/build/x86_64-osx/ghc-8.6.5/haskell-ide-engine-0.10.0.0/x/hie/build/hie/hie -d" fullCaps "/Users/luke/Source/lsp-test" $ do
  doc <- openDoc "src/Language/Haskell/LSP/Test/Parsing.hs" "haskell"
  liftIO $ getLine
  waitForDiagnostics
  replicateM_ 10 $ do
    liftIO $ putStrLn "----editing----"
    let te = TextEdit (Range (Position 5 0) (Position 5 0)) " "
    applyEdit doc te
    sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
    waitForDiagnostics
