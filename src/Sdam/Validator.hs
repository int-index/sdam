{-# LANGUAGE LambdaCase, DerivingStrategies, RecordWildCards,
             GeneralizedNewtypeDeriving, NamedFieldPuns, DeriveGeneric #-}

module Sdam.Validator
  ( ValidationError(..),
    ValidationResult,
    ValidationValue(..),
    validate
  ) where

import Control.Arrow ((***))
import Data.List as List
import Data.Foldable as Foldable
import Data.HashMap.Strict as HashMap
import Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Sequence (Seq)
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

data ValidationValue =
  ValidationValue (Value ValidationValue) |
  SkipValidation
  deriving stock Show

validate :: Schema -> ValidationValue -> ValidationResult
validate Schema{schemaTypes} = vValue Nothing
  where
    vValue ::
      Maybe TyUnion ->
      ValidationValue ->
      ValidationResult
    vValue _ SkipValidation = mempty
    vValue mTyU (ValidationValue (ValueStr tyName _)) =
      vTyName tyName mTyU (\ty -> vStr tyName ty)
    vValue mTyU (ValidationValue (ValueRec tyName fields)) =
      vTyName tyName mTyU (\ty -> vRec tyName ty fields)
    vValue mTyU (ValidationValue (ValueSeq tyName items)) =
      vTyName tyName mTyU (\ty -> vSeq tyName ty items)

    vTyName ::
      TyName ->
      Maybe TyUnion ->
      (Ty -> ValidationResult) ->
      ValidationResult
    vTyName tyName mTyU cont =
      case HashMap.lookup tyName schemaTypes of
        Nothing -> validationError (UnknownTyName tyName)
        Just ty ->
          cont ty <>
          case mTyU of
            Nothing -> mempty
            Just tyU@(TyUnion u) ->
              if HashSet.member tyName u
              then mempty
              else validationError (TypeMismatch tyName tyU)

    vStr ::
      TyName ->
      Ty ->
      ValidationResult
    vStr _ TyStr = mempty
    vStr tyName (TySeq _) = validationError (ExpectedSeqFoundStr tyName)
    vStr tyName (TyRec _) = validationError (ExpectedRecFoundStr tyName)

    vSeq ::
      TyName ->
      Ty ->
      Seq ValidationValue ->
      ValidationResult
    vSeq tyName TyStr _ = validationError (ExpectedStrFoundSeq tyName)
    vSeq tyName (TyRec _) _ = validationError (ExpectedRecFoundSeq tyName)
    vSeq tyName (TySeq itemTyU) items =
      let
        mkPathSegment i = PathSegmentSeq tyName (intToIndex i)
        vSeqItem = vValue (Just itemTyU)
        pathTrieRoot = mempty
        pathTrieChildren =
          HashMap.fromList $
          List.map (mkPathSegment *** vSeqItem) $
          List.zip [0..] (Foldable.toList items)
      in
        PathTrie{pathTrieRoot, pathTrieChildren}
    vRec ::
      TyName ->
      Ty ->
      HashMap FieldName ValidationValue ->
      ValidationResult
    vRec tyName TyStr _ = validationError (ExpectedStrFoundRec tyName)
    vRec tyName (TySeq _) _ = validationError (ExpectedSeqFoundRec tyName)
    vRec tyName (TyRec fieldTys) fields =
      let
        typedFields = HashMap.intersectionWith (,) fieldTys fields
        missingFields = HashMap.keys (HashMap.difference fieldTys typedFields)
        extraFields = HashMap.keys (HashMap.difference fields fieldTys)
        mkPathSegment fieldName = PathSegmentRec tyName fieldName
        vRecField (fieldTyU, field) = vValue (Just fieldTyU) field
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
