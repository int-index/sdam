{-# LANGUAGE LambdaCase, DerivingStrategies, RecordWildCards,
             GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Sdam.Printer
  (
    -- Schema
    rSchema,

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
import qualified Data.HashSet as HashSet
import Text.PrettyPrint

import Sdam.Name
import Sdam.Core

rSchema :: Schema -> Doc
rSchema Schema{schemaTypes} =
  vcat (map rTyDecl (HashMap.toList schemaTypes))

rTyDecl :: (TyName, Ty) -> Doc
rTyDecl (tyName, TyRec fields) | null fields =
  rTyName tyName <> semi
rTyDecl (tyName, ty) =
  hang (rTyName tyName <+> equals) 2 (rTy ty) <> semi

rTyName :: TyName -> Doc
rFieldName :: FieldName -> Doc
rName :: Name -> Doc
rTyName TyName{tyName} = rName tyName
rFieldName FieldName{fieldName} = rName fieldName
rName = text . nameToStr

rTy :: Ty -> Doc
rTy TyStr = text "!str"
rTy (TySeq u) =
  let (uAtom, uDoc) = rTyUnion u
      p = if uAtom then id else parens
  in p uDoc <> char '*'
rTy (TyRec fields) =
  vcat $
  punctuate comma $
  map rTyRecField $
  HashMap.toList fields

rTyRecField :: (FieldName, TyUnion) -> Doc
rTyRecField (fieldName, u) =
  let (_, uDoc) = rTyUnion u
  in rFieldName fieldName <> colon <+> uDoc

rTyUnion :: TyUnion -> (Bool, Doc)
rTyUnion (TyUnion u) =
  case HashSet.toList u of
    [x] -> (True, rTyName x)
    xs ->
      let uDoc =
            hsep $
            punctuate (text " |") $
            map rTyName xs
      in (False, uDoc)

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
