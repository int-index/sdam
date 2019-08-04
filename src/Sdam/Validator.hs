{-# LANGUAGE LambdaCase, DerivingStrategies, RecordWildCards,
             GeneralizedNewtypeDeriving, NamedFieldPuns, DeriveGeneric #-}

module Sdam.Validator
  ( ValidationError(..),
    ValidationResult,
    ValidationValue(..),
    validate
  ) where

import Control.Arrow ((***))
import Data.Text as Text
import Data.List as List
import Data.Foldable as Foldable
import Data.HashMap.Strict as HashMap
import Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Sequence (Seq)
import Text.Regex.Applicative as RE
import GHC.Generics (Generic)
import Sdam.Core

data ValidationError =
  UnknownTyName TyName |
  UnexpectedSeq |
  RegexFail |
  TypeMismatch TyName (HashSet TyName) |
  ExpectedStrFoundRec TyName |
  ExpectedRecFoundStr TyName |
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
    vValue mTyU (ValidationValue (ValueStr tyName str)) =
      vTyName tyName mTyU (\ty -> vStr tyName ty str)
    vValue mTyU (ValidationValue (ValueRec tyName fields)) =
      vTyName tyName mTyU (\ty -> vRec tyName ty fields)
    vValue mTyU (ValidationValue (ValueSeq items)) =
      case mTyU of
        Nothing -> mempty
        Just (TyUnion _ mItemTyU) -> vSeq mItemTyU items

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
            Just (TyUnion u _) ->
              if HashSet.member tyName u
              then mempty
              else validationError (TypeMismatch tyName u)

    vStr ::
      TyName ->
      Ty ->
      Text ->
      ValidationResult
    vStr _ (TyStr re) str =
      case RE.match re (Text.unpack str) of
        Just () -> mempty
        Nothing -> validationError RegexFail
    vStr tyName (TyRec _) _ = validationError (ExpectedRecFoundStr tyName)

    vSeq ::
      Maybe TyUnion ->
      Seq ValidationValue ->
      ValidationResult
    vSeq Nothing _ = validationError UnexpectedSeq
    vSeq (Just itemTyU) items =
      let
        mkPathSegment i = PathSegmentSeq (intToIndex i)
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
    vRec tyName (TyStr _) _ = validationError (ExpectedStrFoundRec tyName)
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
