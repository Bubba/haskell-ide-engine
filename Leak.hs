#!/usr/bin/env cabal
{- cabal:
build-depends: base, lsp-test
-}
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Control.Monad

exe = "/Users/luke/Source/haskell-ide-engine/dist-newstyle/build/x86_64-osx/ghc-8.6.5/haskell-ide-engine-0.10.0.0/x/hie/build/hie/hie"

main = do
  let cfg = defaultConfig { logStdErr = True, messageTimeout = 5000000 }
  runSessionWithConfig cfg (exe <>"") fullCaps "../lsp-test" $ do
    doc <- openDoc "src/Language/Haskell/LSP/Test/Parsing.hs" "haskell"
    waitForDiagnostics
    replicateM_ 30 $ do
      sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
      waitForDiagnostics -- wait before sending it again
