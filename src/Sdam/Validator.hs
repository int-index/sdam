{-# LANGUAGE LambdaCase, DerivingStrategies, RecordWildCards,
             GeneralizedNewtypeDeriving, NamedFieldPuns, DeriveGeneric #-}

module Sdam.Validator
  ( ValidationError(..),
    ValidationResult,
    ValidationObject(..),
    validate
  ) where

import Control.Arrow ((***))
import Data.List as List
import Data.Foldable as Foldable
import Data.HashMap.Strict as HashMap
import Data.HashSet as HashSet
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Sdam.Core

data ValidationError =
  UnknownTyName TyName |
  TypeMismatch TyName TyUnion |
  ExpectedStrFoundSeq TyName |
  ExpectedStrFoundRec TyName |
  ExpectedRecFoundStr TyName |
  ExpectedRecFoundSeq TyName |
  ExpectedSeqFoundStr TyName |
  ExpectedSeqFoundRec TyName |
  RecMissingField FieldName |
  RecExtraField FieldName
  deriving (Eq, Show, Generic)

instance Hashable ValidationError

type ValidationResult = PathTrie (HashSet ValidationError)

validationError :: ValidationError -> ValidationResult
validationError e = mempty{ pathTrieRoot = HashSet.singleton e }

data ValidationObject =
  ValidationObject (Object ValidationObject) |
  SkipValidation
  deriving stock Show

validate :: Schema -> ValidationObject -> ValidationResult
validate Schema{schemaTypes} = vObject Nothing
  where
    vObject ::
      Maybe TyUnion ->
      ValidationObject ->
      ValidationResult
    vObject _ SkipValidation = mempty
    vObject mTyU (ValidationObject (Object tyName value)) =
      case HashMap.lookup tyName schemaTypes of
        Nothing -> validationError (UnknownTyName tyName)
        Just ty ->
          maybe mempty (vObjectTyName tyName) mTyU <>
          vValue (tyName, ty) value
    vObjectTyName ::
      TyName ->
      TyUnion ->
      ValidationResult
    vObjectTyName tyName tyU@(TyUnion u)
      | HashSet.member tyName u = mempty
      | otherwise = validationError (TypeMismatch tyName tyU)
    vValue ::
      (TyName, Ty) ->
      Value ValidationObject ->
      ValidationResult
    vValue (tyName, TyStr) =
      \case
        ValueStr _ -> mempty
        ValueSeq _ -> validationError (ExpectedStrFoundSeq tyName)
        ValueRec _ -> validationError (ExpectedStrFoundRec tyName)
    vValue (tyName, TySeq itemTyU) =
      \case
        ValueSeq items -> vSeq tyName itemTyU (Foldable.toList items)
        ValueRec _ -> validationError (ExpectedSeqFoundRec tyName)
        ValueStr _ -> validationError (ExpectedSeqFoundStr tyName)
    vValue (tyName, TyRec fieldTys) =
      \case
        ValueRec fields -> vRec tyName fieldTys fields
        ValueSeq _ -> validationError (ExpectedRecFoundSeq tyName)
        ValueStr _ -> validationError (ExpectedRecFoundStr tyName)
    vSeq ::
      TyName ->
      TyUnion ->
      [ValidationObject] ->
      ValidationResult
    vSeq tyName itemTyU items =
      let
        mkPathSegment i = PathSegmentSeq tyName (intToIndex i)
        vSeqItem = vObject (Just itemTyU)
        pathTrieRoot = mempty
        pathTrieChildren =
          HashMap.fromList $
          List.map (mkPathSegment *** vSeqItem) $
          List.zip [0..] items
      in
        PathTrie{pathTrieRoot, pathTrieChildren}
    vRec ::
      TyName ->
      HashMap FieldName TyUnion ->
      HashMap FieldName ValidationObject ->
      ValidationResult
    vRec tyName fieldTys fields =
      let
        typedFields = HashMap.intersectionWith (,) fieldTys fields
        missingFields = HashMap.keys (HashMap.difference fieldTys typedFields)
        extraFields = HashMap.keys (HashMap.difference fields fieldTys)
        mkPathSegment fieldName = PathSegmentRec tyName fieldName
        vRecField (fieldTyU, field) = vObject (Just fieldTyU) field
        pathTrieRoot =
          HashSet.fromList $
          List.map RecMissingField missingFields <>
          List.map RecExtraField extraFields
        pathTrieChildren =
          HashMap.fromList $
          List.map (mkPathSegment *** vRecField) $
          HashMap.toList typedFields
      in
        PathTrie{pathTrieRoot, pathTrieChildren}
