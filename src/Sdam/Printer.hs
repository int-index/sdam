{-# LANGUAGE LambdaCase, DerivingStrategies, RecordWildCards,
             GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Sdam.Printer
  (
    -- Value
    rValue,
    RenderValue(..),

    -- Path
    rPath,

    -- Running
    render
  ) where

import Prelude hiding ((<>))

import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import Text.PrettyPrint

import Sdam.Name
import Sdam.Core

rTyName :: TyName -> Doc
rFieldName :: FieldName -> Doc
rName :: Name -> Doc
rTyName (TyName name) = rName name
rFieldName (FieldName name) = rName name
rName = text . nameToStr

newtype RenderValue = RenderValue (Value RenderValue)
  deriving newtype Show

rValue :: RenderValue -> Doc
rValue (RenderValue (ValueStr tyName s)) =
  rTyName tyName <+> text (show s)
rValue (RenderValue (ValueSeq tyName xs)) =
  (<+>) (rTyName tyName) $
  brackets . sep . punctuate comma $
  map rValue (toList xs)
rValue (RenderValue (ValueRec tyName fields)) =
  (<+>) (rTyName tyName) $
  braces . sep . punctuate comma $
  map rRecField (HashMap.toList fields)

rRecField :: (FieldName, RenderValue) -> Doc
rRecField (fieldName, value) =
  rFieldName fieldName <+> equals <+> rValue value

rPath :: Path -> Doc
rPath (Path ps) =
  hcat $
  punctuate (char '/') $
  map rPathSegment ps

rPathSegment :: PathSegment -> Doc
rPathSegment (PathSegmentRec tyName fieldName) =
  rTyName tyName <> char '.' <> rFieldName fieldName
rPathSegment (PathSegmentSeq tyName i) =
  rTyName tyName <> brackets (int (indexToInt i))
