{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving,
             NamedFieldPuns #-}

module Sdam.MetaVar where

import Data.Set (Set)
import qualified Data.Set as Set

import Sdam.Name
import Sdam.Core

newtype MetaVar = MetaVar { metaVarName :: Name }
  deriving newtype (Eq, Ord)

metaVarStr :: MetaVar -> String
metaVarStr MetaVar{metaVarName} = nameToStr metaVarName

data Ty' =
  TyRec' [(FieldName, TyU)] |
  TySeq' TyU |
  TyStr'

data TyU =
  TyU
    { tyU_metaVars :: Set MetaVar,
      tyU_tyNames :: Set TyName }

instance Semigroup TyU where
  TyU mv1 tn1 <> TyU mv2 tn2 =
    TyU (Set.union mv1 mv2) (Set.union tn1 tn2)

instance Monoid TyU where
  mempty = TyU Set.empty Set.empty

tyU_MetaVar :: MetaVar -> TyU
tyU_MetaVar mv = TyU (Set.singleton mv) Set.empty

tyU_TyName :: TyName -> TyU
tyU_TyName tn = TyU Set.empty (Set.singleton tn)
