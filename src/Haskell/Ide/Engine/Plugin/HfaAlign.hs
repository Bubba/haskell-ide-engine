{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- Simple example plugin showing how easy it is to make a plugin, using the operations from
-- http://www.haskellforall.com/2018/10/detailed-walkthrough-for-beginner.html
module Haskell.Ide.Engine.Plugin.HfaAlign where

import           Control.Lens
import           Data.Aeson
import qualified Data.HashMap.Strict           as H
import qualified GHC.Generics                  as Generics
import           Haskell.Ide.Engine.MonadTypes hiding (_range)
import           Haskell.Ide.Engine.PluginUtils
import qualified Language.Haskell.LSP.Types      as J
import qualified Language.Haskell.LSP.Types.Lens as J

-- blog post imports
import Data.Text (Text)

import qualified Data.Text
-- import qualified Data.Text.IO
import qualified Safe

-- ---------------------------------------------------------------------

hfaAlignDescriptor :: PluginDescriptor
hfaAlignDescriptor = PluginDescriptor
  { pluginId = "hfaa"
  , pluginCommands = [ PluginCommand "align" alignCmd ]
  , pluginCodeActionProvider = Just codeActionProvider
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }

-- ---------------------------------------------------------------------

data AlignParams = AlignParams
  { file  :: Uri
  , range :: J.Range
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

alignCmd :: AlignParams -> IdeGhcM (IdeResult J.WorkspaceEdit)
alignCmd (AlignParams uri rg) = do
  mtext <- getRangeFromVFS uri rg
  case mtext of
    Nothing -> return $ IdeResultOk $ J.WorkspaceEdit Nothing Nothing
    Just txt -> do
      let
        adjusted = adjustText txt
        textEdits = J.List [J.TextEdit rg adjusted ]
        res = J.WorkspaceEdit
          (Just $ H.singleton uri textEdits)
          Nothing
      return $ IdeResultOk res

-- ---------------------------------------------------------------------

codeActionProvider :: CodeActionProvider
codeActionProvider plId docId (Range (Position sl _) (Position el _)) _context = do
  cmd <- mkLspCommand plId "align" title  (Just cmdParams)
  return $ IdeResultOk [codeAction cmd]
  where
    codeAction :: J.Command -> J.CodeAction
    codeAction cmd = J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [])) Nothing (Just cmd)
    title = "Align on ="
    cmdParams = [toJSON (AlignParams (docId ^. J.uri) (Range (Position sl 0) (Position (el+1) 0)) )]

-- ---------------------------------------------------------------------
-- Blog post code
-- ---------------------------------------------------------------------

prefixLength :: Text -> Int
prefixLength line = Data.Text.length prefix
  where
    (prefix, _suffix) = Data.Text.breakOn "=" line

adjustLine :: Int -> Text -> Text
adjustLine desiredPrefixLength oldLine = newLine
  where
    (prefix, suffix) = Data.Text.breakOn "=" oldLine

    actualPrefixLength = Data.Text.length prefix

    additionalSpaces = desiredPrefixLength - actualPrefixLength

    spaces = Data.Text.replicate additionalSpaces " "

    newLine = Data.Text.concat [ prefix, spaces, suffix ]

adjustText :: Text -> Text
adjustText oldText = newText
  where
    oldLines = Data.Text.lines oldText

    prefixLengths = map prefixLength oldLines

    newLines =
        case Safe.maximumMay prefixLengths of
            Nothing ->
                []
            Just desiredPrefixLength ->
                map (adjustLine desiredPrefixLength) oldLines

    newText = Data.Text.unlines newLines

-- main :: IO ()
-- main = Data.Text.IO.interact adjustText
