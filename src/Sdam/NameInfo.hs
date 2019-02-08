{-# LANGUAGE DerivingStrategies, NamedFieldPuns #-}

module Sdam.NameInfo
  ( EnvNameInfo(..),
    buildEnvNameInfo
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Sdam.Core

data EnvNameInfo =
  EnvNameInfo
    { envNameInfoTypes :: Map TyId TyName,
      envNameInfoFields :: Map FieldId (TyName, FieldName)
    }
  deriving stock Show

buildEnvNameInfo :: Env -> EnvNameInfo
buildEnvNameInfo env =
  let
    envNameInfoTypes =
      Map.fromList
        [ (mkTyId tyName, tyName) |
          tyName <- Map.keys (envMap env) ]
    envNameInfoFields =
      Map.fromList
        [ (mkFieldId tyName fieldName, (tyName, fieldName)) |
          (tyName, TyRec fields) <- Map.toList (envMap env),
          fieldName <- Map.keys fields ]
  in
    EnvNameInfo{envNameInfoTypes, envNameInfoFields}
