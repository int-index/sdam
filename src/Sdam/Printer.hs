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
import Data.Sequence.NonEmpty (NonEmptySeq)
import Text.PrettyPrint

import Sdam.Name
import Sdam.Core

rTyName :: TyName -> Doc
rFieldName :: FieldName -> Doc
rName :: Name -> Doc
rTyName TyName{tyName} = rName tyName
rFieldName FieldName{fieldName} = rName fieldName
rName = text . nameToStr

newtype RenderObject = RenderObject (Object RenderObject)
  deriving newtype Show

rObject :: RenderObject -> Doc
rObject (RenderObject (Object tyName v)) =
  rTyName tyName <+> rValue v

rValue :: Value RenderObject -> Doc
rValue (ValueStr s) = text (show s)
rValue (ValueRec fields) =
  braces . sep . punctuate comma $
  concatMap rRecField (HashMap.toList fields)

rRecField :: (FieldName, NonEmptySeq RenderObject) -> [Doc]
rRecField (fieldName, objects) =
    map rFieldObject (toList objects)
  where
    rFieldObject object =
      rFieldName fieldName <+> equals <+> rObject object

rPath :: Path -> Doc
rPath (Path ps) =
  hcat $
  punctuate (char '/') $
  map rPathSegment ps

rPathSegment :: PathSegment -> Doc
rPathSegment (PathSegment tyName fieldName i) =
  rTyName tyName
    <> char '.' <> rFieldName fieldName
    <> brackets (int (indexToInt i))
