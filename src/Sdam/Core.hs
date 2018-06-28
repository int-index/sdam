{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Sdam.Core
  (
    -- * Types
    TyName(..),
    FieldName(..),
    Env(..),
    Ty(..),
    TyUnion(..),

    -- * Values
    TyId(..),
    mkTyId,
    FieldId(..),
    mkFieldId,
    Ref(..),
    Space(..),
    Object(..),
    Value(..),

    -- * Paths
    Path(..),
    PathSegment(..),
    Index(..)
  ) where

import Data.Word (Word)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Fingerprint

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype TyName = TyName { tyNameStr :: String }
  deriving newtype (Eq, Ord, Show)

newtype FieldName = FieldName { fieldNameStr :: String }
  deriving newtype (Eq, Ord, Show)

newtype Env = Env { envMap :: Map TyName Ty }
  deriving newtype Show

data Ty =
  TyRec (Map FieldName TyUnion) |
  TySeq TyUnion
  deriving stock Show

data TyUnion = TyUnion (Set TyName)
  deriving stock Show

instance Semigroup TyUnion where
  TyUnion tns1 <> TyUnion tns2 = TyUnion (Set.union tns1 tns2)

instance Monoid TyUnion where
  mempty = TyUnion Set.empty

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

newtype TyId = TyId Fingerprint
  deriving newtype (Eq, Ord)

mkTyId :: TyName -> TyId
mkTyId (TyName s) = TyId (fingerprintString s)

newtype FieldId = FieldId Fingerprint
  deriving newtype (Eq, Ord)

mkFieldId :: TyName -> FieldName -> FieldId
mkFieldId ty (FieldName s) =
  let TyId tyFp = mkTyId ty
  in FieldId (fingerprintFingerprints [tyFp, fingerprintString s])

newtype Ref = Ref Word
  deriving newtype (Eq, Ord)

data Space p =
  Space
    { spaceMap :: Map Ref (Object p Ref),
      spaceNextRef :: Ref }

data Object p a = Object TyId (Value p a)

data Value p a =
  ValueRec (Map FieldId a) |
  ValueSeq [a] |
  ValuePrim p

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

newtype Path = Path [PathSegment]

data PathSegment =
  PathSegmentRec FieldId |
  PathSegmentSeq Index

newtype Index = Index Word
