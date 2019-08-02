{-# LANGUAGE LambdaCase, DerivingStrategies, RecordWildCards,
             GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Sdam.Printer
  (
    -- Object/Value
    rObject,
    RenderObject(..),

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

newtype RenderObject = RenderObject (Object RenderObject)
  deriving newtype Show

rObject :: RenderObject -> Doc
rObject (RenderObject (Object tyName v)) =
  rTyName tyName <+> rValue v

rValue :: Value RenderObject -> Doc
rValue (ValueStr s) = text (show s)
rValue (ValueSeq xs) =
  brackets . sep . punctuate comma $
  map rObject (toList xs)
rValue (ValueRec fields) =
  braces . sep . punctuate comma $
  map rRecField (HashMap.toList fields)

rRecField :: (FieldName, RenderObject) -> Doc
rRecField (fieldName, object) =
  rFieldName fieldName <+> equals <+> rObject object

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
