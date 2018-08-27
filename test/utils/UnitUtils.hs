module UnitUtils where

import           Data.Aeson
import           Data.Default
import           Data.Typeable
import qualified Data.Map as Map
import qualified GhcMod.Monad as GM
import qualified GhcMod.Types as GM
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Test.Hspec
  

testCommand :: (ToJSON a, Typeable b, ToJSON b, Show b, Eq b) => IdePlugins -> IdeGhcM (IdeResult b) -> PluginId -> CommandName -> a -> IdeResult b -> IO ()
testCommand testPlugins act plugin cmd arg res = do
  (newApiRes, oldApiRes) <- runIGM testPlugins $ do
    new <- act
    old <- makeRequest plugin cmd arg
    return (new, old)
  newApiRes `shouldBe` res
  fmap fromDynJSON oldApiRes `shouldBe` fmap Just res

runSingleReq :: ToJSON a => IdePlugins -> PluginId -> CommandName -> a -> IO (IdeResult DynamicJSON)
runSingleReq testPlugins plugin com arg = runIGM testPlugins (makeRequest plugin com arg)

makeRequest :: ToJSON a => PluginId -> CommandName -> a -> IdeGhcM (IdeResult DynamicJSON)
makeRequest plugin com arg = runPluginCommand plugin com (toJSON arg)

runIGM :: IdePlugins -> IdeGhcM a -> IO a
runIGM testPlugins = runIdeGhcM testOptions def (IdeState emptyModuleCache Map.empty testPlugins Map.empty Nothing 0)

testOptions :: GM.Options
testOptions = GM.defaultOptions {
    GM.optOutput     = GM.OutputOpts {
      GM.ooptLogLevel       = GM.GmError
      -- GM.ooptLogLevel       = GM.GmVomit
    , GM.ooptStyle          = GM.PlainStyle
    , GM.ooptLineSeparator  = GM.LineSeparator "\0"
    , GM.ooptLinePrefix     = Nothing
    }

    }