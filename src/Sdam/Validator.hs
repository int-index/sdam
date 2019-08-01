{-# LANGUAGE LambdaCase, DerivingStrategies, RecordWildCards,
             GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Sdam.Validator
  ( ValidationError(..),
    ValidationResult,
    ValidationObject(..),
    validate
  ) where

import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
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

type ValidationResult = [(Path, ValidationError)]

newtype ValidationObject = ValidationObject (Object ValidationObject)
  deriving newtype Show

validate :: Schema -> ValidationObject -> ValidationResult
validate Schema{schemaTypes} = vObject mempty Nothing
  where
    vObject ::
      PathBuilder ->
      Maybe TyUnion ->
      ValidationObject ->
      ValidationResult
    vObject pb mTyU (ValidationObject (Object tyName value)) =
      case HashMap.lookup tyName schemaTypes of
        Nothing -> [(buildPath pb, UnknownTyName tyName)]
        Just ty ->
          maybe [] (vObjectTyName pb tyName) mTyU ++
          vValue pb (tyName, ty) value
    vObjectTyName ::
      PathBuilder ->
      TyName ->
      TyUnion ->
      ValidationResult
    vObjectTyName pb tyName tyU@(TyUnion u)
      | HashSet.member tyName u = []
      | otherwise = [(buildPath pb, TypeMismatch tyName tyU)]
    vValue ::
      PathBuilder ->
      (TyName, Ty) ->
      Value ValidationObject ->
      ValidationResult
    vValue pb (tyName, TyStr) =
      \case
        ValueStr _ -> []
        ValueSeq _ -> [(buildPath pb, ExpectedStrFoundSeq tyName)]
        ValueRec _ -> [(buildPath pb, ExpectedStrFoundRec tyName)]
    vValue pb (tyName, TySeq itemTy) =
      \case
        ValueSeq items -> vSeq pb tyName itemTy (toList items)
        ValueRec _ -> [(buildPath pb, ExpectedSeqFoundRec tyName)]
        ValueStr _ -> [(buildPath pb, ExpectedSeqFoundStr tyName)]
    vValue pb (tyName, TyRec fieldTys) =
      \case
        ValueRec fields -> vRec pb tyName fieldTys fields
        ValueSeq _ -> [(buildPath pb, ExpectedRecFoundSeq tyName)]
        ValueStr _ -> [(buildPath pb, ExpectedRecFoundStr tyName)]
    vSeq ::
      PathBuilder ->
      TyName ->
      TyUnion ->
      [ValidationObject] ->
      ValidationResult
    vSeq pb tyName itemTy items =
      let
        pb' i = pb <> mkPathBuilder (PathSegmentSeq tyName i)
        vSeqItem (i, item) = vObject (pb' i) (Just itemTy) item
      in
        concatMap vSeqItem (enumerate items)
    vRec ::
      PathBuilder ->
      TyName ->
      HashMap FieldName TyUnion ->
      HashMap FieldName ValidationObject ->
      ValidationResult
    vRec pb tyName fieldTys fields =
      let
        p = buildPath pb
        pb' fieldName = pb <> mkPathBuilder (PathSegmentRec tyName fieldName)
        typedFields = HashMap.intersectionWith (,) fieldTys fields
        missingFields = HashMap.keys (HashMap.difference fieldTys typedFields)
        extraFields = HashMap.keys (HashMap.difference fields fieldTys)
        vRecField (fieldName, (fieldTy, field)) =
          vObject (pb' fieldName) (Just fieldTy) field
      in
        map (\fieldName -> (p, RecMissingField fieldName)) missingFields ++
        map (\fieldName -> (p, RecExtraField fieldName)) extraFields ++
        concatMap vRecField (HashMap.toList typedFields)

enumerate :: [a] -> [(Index, a)]
enumerate = zip (map intToIndex [0..])
