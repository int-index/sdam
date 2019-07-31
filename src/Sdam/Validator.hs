{-# LANGUAGE LambdaCase, DerivingStrategies, RecordWildCards,
             GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Sdam.Validator
  ( ValidationError(..),
    ValidationResult,
    ValidationObject(..),
    validate
  ) where

import Data.Foldable (toList, length)
import Data.HashMap.Strict (HashMap)
import Data.Sequence.NonEmpty (NonEmptySeq)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Sdam.Core

data ValidationError =
  UnknownTyName TyName |
  TypeMismatch TyName TyUnion |
  ExpectedStrFoundRec TyName |
  ExpectedRecFoundStr TyName |
  RecMissingField FieldName |
  RecExtraField FieldName |
  RecRepeatedField FieldName

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
        ValueRec _ -> [(buildPath pb, ExpectedStrFoundRec tyName)]
    vValue pb (tyName, TyRec fieldTys) =
      \case
        ValueRec fields -> vRec pb tyName fieldTys fields
        ValueStr _ -> [(buildPath pb, ExpectedRecFoundStr tyName)]
    vRec ::
      PathBuilder ->
      TyName ->
      HashMap FieldName FieldTy ->
      HashMap FieldName (NonEmptySeq ValidationObject) ->
      ValidationResult
    vRec pb tyName fieldTys fields =
      let
        p = buildPath pb
        pb' fieldName i = pb <> mkPathBuilder (PathSegment tyName fieldName i)
        typedFields = HashMap.intersectionWith (,) fieldTys fields
        requiredFieldTys = HashMap.filter isRequiredFieldTy fieldTys
        missingFields = HashMap.keys (HashMap.difference requiredFieldTys typedFields)
        extraFields = HashMap.keys (HashMap.difference fields fieldTys)
        vRecField (fieldName, (FieldTy tyU mult, objects)) =
          vMult fieldName mult (length objects) ++
          concatMap
            (\(i,obj) -> vObject (pb' fieldName i) (Just tyU) obj)
            (enumerate (toList objects))
        vMult fieldName Mult{multUpper} n =
          case multUpper of
            UpperBoundInf -> []
            UpperBoundOne ->
              if n > 1 then [(p, RecRepeatedField fieldName)] else []
      in
        map (\fieldName -> (p, RecMissingField fieldName)) missingFields ++
        map (\fieldName -> (p, RecExtraField fieldName)) extraFields ++
        concatMap vRecField (HashMap.toList typedFields)

isRequiredFieldTy :: FieldTy -> Bool
isRequiredFieldTy (FieldTy _ Mult{multLower}) =
  case multLower of
    LowerBoundZero -> False
    LowerBoundOne -> True

enumerate :: [a] -> [(Index, a)]
enumerate = zip (map intToIndex [0..])
