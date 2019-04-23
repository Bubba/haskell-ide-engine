{-# LANGUAGE OverloadedStrings #-}
module ExtensibleStateSpec where

import qualified Data.Text                           as T
import           Data.Typeable
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.MonadFunctions
import           TestUtils

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ExtensibleState" extensibleStateSpec

extensibleStateSpec :: Spec
extensibleStateSpec =
  describe "stores and retrieves in the state" $
    it "stores the first one" $ do
      r <- runIGM testPlugins $ do
          r1 <- makeRequest "test" "cmd1" ()
          r2 <- makeRequest "test" "cmd2" ()
          return (r1,r2)
      fmap fromDynJSON (fst r) `shouldBe` IdeResultOk (Just "result:put foo" :: Maybe T.Text)
      fmap fromDynJSON (snd r) `shouldBe` IdeResultOk (Just "result:got:\"foo\"" :: Maybe T.Text)

-- ---------------------------------------------------------------------

testPlugins :: IdePlugins
testPlugins = mkIdePlugins [testDescriptor]

testDescriptor :: PluginDescriptor
testDescriptor = PluginDescriptor
  { pluginId = "test"
  , pluginCommands = [
        PluginCommand "cmd1" cmd1
      , PluginCommand "cmd2" cmd2
      ]
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }

-- ---------------------------------------------------------------------

cmd1 :: () -> IdeGhcM (IdeResult T.Text)
cmd1 _ = do
  put (MS1 "foo")
  return (IdeResultOk (T.pack "result:put foo"))

cmd2 :: () -> IdeGhcM (IdeResult T.Text)
cmd2 _ = do
  (MS1 v) <- get
  return (IdeResultOk (T.pack $ "result:got:" ++ show v))

newtype MyState1 = MS1 T.Text deriving Typeable

instance ExtensionClass MyState1 where
  initialValue = MS1 "initial"

-- ---------------------------------------------------------------------
