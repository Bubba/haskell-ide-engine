{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haskell.Ide.Engine.PluginsIdeMonads
import Haskell.Ide.Engine.Plugin.GhcMod
import System.Directory
import Language.Haskell.LSP.Types
import TestUtils

main :: IO ()
main = withCurrentDirectory "./test/testdata" $ do
    fp <- makeAbsolute "./FileWithWarning.hs"
    let testPlugins = pluginDescToIdePlugins [ghcmodDescriptor "ghcmod"]
    (runIGM testPlugins $ setTypecheckedModule (filePathToUri fp)) >>= print
    print "done"