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
validate Schema{schemaTypes, schemaRoot} = vValue schemaRoot
  where
    vValue ::
      TyUnion ->
      ValidationValue ->
      ValidationResult
    vValue _ SkipValidation = mempty
    vValue tyU (ValidationValue (ValueStr tyName str)) =
      vTyName tyName tyU (\tyInst -> vStr tyName tyInst str)
    vValue tyU (ValidationValue (ValueRec tyName fields)) =
      vTyName tyName tyU (\tyInst -> vRec tyName tyInst fields)
    vValue (TyUnion _ mItemTyU) (ValidationValue (ValueSeq items)) =
      case mItemTyU of
        Nothing -> validationError UnexpectedSeq
        Just itemTyU -> vSeq itemTyU items

    vTyName ::
      TyName ->
      TyUnion ->
      (TyInst -> ValidationResult) ->
      ValidationResult
    vTyName tyName (TyUnion u _) cont =
      case HashMap.lookup tyName schemaTypes of
        Nothing -> validationError (UnknownTyName tyName)
        Just tyDefn ->
          case HashMap.lookup tyName u of
            Nothing -> validationError (TypeMismatch tyName (HashMap.keysSet u))
            Just tyInst -> cont (checkTyInst tyName tyDefn tyInst)

    vStr ::
      TyName ->
      TyInst ->
      Text ->
      ValidationResult
    vStr _ (TyInstStr re) str =
      case RE.match re (Text.unpack str) of
        Just () -> mempty
        Nothing -> validationError RegexFail
    vStr tyName (TyInstRec _) _ = validationError (ExpectedRecFoundStr tyName)

    vSeq ::
      TyUnion ->
      Seq ValidationValue ->
      ValidationResult
    vSeq itemTyU items =
      let
        mkPathSegment i = PathSegmentSeq (intToIndex i)
        vSeqItem = vValue itemTyU
        pathTrieRoot = mempty
        pathTrieChildren =
          HashMap.fromList $
          List.map (mkPathSegment *** vSeqItem) $
          List.zip [0..] (Foldable.toList items)
      in
        PathTrie{pathTrieRoot, pathTrieChildren}

    vRec ::
      TyName ->
      TyInst ->
      HashMap FieldName ValidationValue ->
      ValidationResult
    vRec tyName (TyInstStr _) _ = validationError (ExpectedStrFoundRec tyName)
    vRec tyName (TyInstRec fieldTys) fields =
      let
        typedFields = HashMap.intersectionWith (,) fieldTys fields
        missingFields = HashMap.keys (HashMap.difference fieldTys typedFields)
        extraFields = HashMap.keys (HashMap.difference fields fieldTys)
        mkPathSegment fieldName = PathSegmentRec tyName fieldName
        vRecField (fieldTyU, field) = vValue fieldTyU field
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
