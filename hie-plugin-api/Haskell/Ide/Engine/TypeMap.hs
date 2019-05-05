{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module Haskell.Ide.Engine.TypeMap where

import qualified Data.IntervalMap.FingerTree   as IM

import qualified GHC
import           GHC                            ( TypecheckedModule )

import           Data.Data                     as Data
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified TcHsSyn
import qualified CoreUtils
import qualified Type
import qualified Desugar
import           Haskell.Ide.Engine.Compat

import           Haskell.Ide.Engine.ArtifactMap

-- | Generate a mapping from an Interval to types.
-- Intervals may overlap and return more specific results.
genTypeMap :: GHC.GhcMonad m => TypecheckedModule -> m TypeMap
genTypeMap tm = do
  let typecheckedSource = GHC.tm_typechecked_source tm
  hs_env <- GHC.getSession
  liftIO $ types hs_env typecheckedSource


everythingInTypecheckedSourceM
  :: Data x => (forall a . Data a => a -> IO TypeMap) -> x -> IO TypeMap
everythingInTypecheckedSourceM = everythingButTypeM @GHC.Id


-- | Obtain details map for types.
types :: GHC.HscEnv -> GHC.TypecheckedSource -> IO TypeMap
types hs_env = everythingInTypecheckedSourceM (ty `combineM` fun `combineM` funBind)
 where
  ty :: forall a . Data a => a -> IO TypeMap
  ty term = case cast term of
    (Just lhsExprGhc@(GHC.L (GHC.RealSrcSpan spn) _)) ->
      getType hs_env lhsExprGhc >>= \case
        Nothing       -> return IM.empty
        Just (_, typ) -> return (IM.singleton (rspToInt spn) typ)
    _ -> return IM.empty

  fun :: forall a . Data a => a -> IO TypeMap
  fun term = case cast term of
    (Just (GHC.L (GHC.RealSrcSpan spn) hsPatType)) ->
      return (IM.singleton (rspToInt spn) (TcHsSyn.hsPatType hsPatType))
    _ -> return IM.empty

  funBind :: forall a . Data a => a -> IO TypeMap
  funBind term = case cast term of
    (Just (GHC.L (GHC.RealSrcSpan spn) (FunBindType t))) ->
      return (IM.singleton (rspToInt spn) t)
    _ -> return IM.empty

-- | Combine two queries into one using alternative combinator.
combineM
  :: (forall a . Data a => a -> IO TypeMap)
  -> (forall a . Data a => a -> IO TypeMap)
  -> (forall a . Data a => a -> IO TypeMap)
combineM f g x = do
  a <- f x
  b <- g x
  return (a `IM.union` b)

-- | Variation of "everything" that does not recurse into children of type t
-- requires AllowAmbiguousTypes
everythingButTypeM
  :: forall t
   . (Typeable t)
  => (forall a . Data a => a -> IO TypeMap)
  -> (forall a . Data a => a -> IO TypeMap)
everythingButTypeM f = everythingButM $ (,) <$> f <*> isType @t

-- | Returns true if a == t.
-- requires AllowAmbiguousTypes
isType :: forall a b . (Typeable a, Typeable b) => b -> Bool
isType _ = isJust $ eqT @a @b

-- | Variation of "everything" with an added stop condition
-- Just like 'everything', this is stolen from SYB package.
everythingButM
  :: (forall a . Data a => a -> (IO TypeMap, Bool))
  -> (forall a . Data a => a -> IO TypeMap)
everythingButM f x = do
  let (v, stop) = f x
  if stop
    then v
    else Data.gmapQr
      (\e acc -> do
        e' <- e
        a  <- acc
        return (e' `IM.union` a)
      )
      v
      (everythingButM f)
      x

-- | Attempts to get the type for expressions in a lazy and cost saving way.
-- Avoids costly desugaring of Expressions and only obtains the type at the leaf of an expression.
--
-- Implementation is taken from: HieAst.hs<https://gitlab.haskell.org/ghc/ghc/blob/1f5cc9dc8aeeafa439d6d12c3c4565ada524b926/compiler/hieFile/HieAst.hs>
-- Slightly adapted to work for the supported GHC versions 8.2.1 - 8.6.4
--
-- See #16233<https://gitlab.haskell.org/ghc/ghc/issues/16233>
getType
  :: GHC.HscEnv -> GHC.LHsExpr GhcTc -> IO (Maybe (GHC.SrcSpan, Type.Type))
getType hs_env e@(GHC.L spn e') =
  -- Some expression forms have their type immediately available
  let
    tyOpt = case e' of
      HsOverLitType t -> Just t
      HsLitType t -> Just t
      HsLamType t -> Just t
      HsLamCaseType t -> Just t
      HsCaseType t -> Just t
      ExplicitListType t -> Just t
      ExplicitSumType t -> Just t
      HsMultiIfType t -> Just t

      _                        -> Nothing
  in  case tyOpt of
        Just t -> return $ Just (spn ,t)
        Nothing
          | skipDesugaring e' -> pure Nothing
          | otherwise -> do
            (_, mbe) <- Desugar.deSugarExpr hs_env e
            let res = (spn, ) . CoreUtils.exprType <$> mbe
            pure res
 where
  -- | Skip desugaring of these expressions for performance reasons.
  --
  -- See impact on Haddock output (esp. missing type annotations or links)
  -- before marking more things here as 'False'. See impact on Haddock
  -- performance before marking more things as 'True'.
  skipDesugaring :: GHC.HsExpr a -> Bool
  skipDesugaring expression = case expression of
    GHC.HsVar{}        -> False
    GHC.HsUnboundVar{} -> False
    GHC.HsConLikeOut{} -> False
    GHC.HsRecFld{}     -> False
    GHC.HsOverLabel{}  -> False
    GHC.HsIPVar{}      -> False
    GHC.HsWrap{}       -> False
    _                  -> True
