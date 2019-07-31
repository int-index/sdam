{-# LANGUAGE LambdaCase, DerivingStrategies, RecordWildCards,
             GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Sdam.Parser
  (
    -- Object/Value
    pObject,
    ParsedObject(..),

    -- Path
    pPath,

    -- Running
    parse
  ) where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import GHC.Exts (IsList(fromList))
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.NonEmpty as NonEmpty

import Sdam.Name
import Sdam.Core

type Parser e = Parsec e String

pWhitespace :: Ord e => Parser e ()
pWhitespace = L.space space1 pLineComment pBlockComment
  where
    pLineComment = L.skipLineComment "//"
    pBlockComment = L.skipBlockCommentNested "/*" "*/"

pLexeme :: Ord e => Parser e a -> Parser e a
pLexeme = L.lexeme pWhitespace

pSymbol :: Ord e => String -> Parser e ()
pSymbol s = void (L.symbol pWhitespace s)

pComma :: Ord e => Parser e ()
pComma = pSymbol ","

pLetter :: Ord e => Parser e Letter
pLetter = unsafeCharToLetter <$> letterChar

pName :: Ord e => Parser e Name
pName = Name <$> (NonEmpty.some pLetter `NonEmpty.sepBy1` char '-')

pTyName :: Ord e => Parser e TyName
pTyName = pLexeme (TyName <$> pName)

pFieldName :: Ord e => Parser e FieldName
pFieldName = pLexeme (FieldName <$> pName)

--------------------------------------------------------------------------------
-- Object
--------------------------------------------------------------------------------

type ObjectParseErr = Void

newtype ParsedObject = ParsedObject (Object ParsedObject)
  deriving newtype Show

pObject :: Parser ObjectParseErr ParsedObject
pObject = do
  tyName <- pTyName
  v <- pValue
  return (ParsedObject (Object tyName v))

pValue :: Parser ObjectParseErr (Value ParsedObject)
pValue =
  ValueStr <$> pValueStr <|>
  ValueSeq <$> pValueSeq <|>
  ValueRec <$> pValueRec

pValueSeq :: Parser ObjectParseErr (Seq ParsedObject)
pValueSeq =
  between (pSymbol "[") (pSymbol "]") $
  fromList <$> (pObject `sepBy` pComma)

pValueRec :: Parser ObjectParseErr (HashMap FieldName ParsedObject)
pValueRec =
  between (pSymbol "{") (pSymbol "}") $
  HashMap.fromList <$> (pFieldDef `sepBy` pComma)

pValueStr :: Parser ObjectParseErr Text
pValueStr = pLexeme $ do
  void (char '\"')
  Text.pack . catMaybes <$> manyTill pChar (char '\"')
  where
    pChar :: Parser ObjectParseErr (Maybe Char)
    pChar =
      (Just <$> L.charLiteral) <|>
      (Nothing <$ string "\\&") <|>
      (Just <$> anySingle)

pFieldDef :: Parser ObjectParseErr (FieldName, ParsedObject)
pFieldDef = do
  fieldName <- pFieldName
  pSymbol "="
  object <- pObject
  return (fieldName, object)

--------------------------------------------------------------------------------
-- Object
--------------------------------------------------------------------------------

type PathParseErr = Void

pPath :: Parser PathParseErr Path
pPath = Path <$> (pPathSegment `sepBy` char '/')

pPathSegment :: Parser PathParseErr PathSegment
pPathSegment = do
  tyName <- TyName <$> pName
  let
    pRec =
      PathSegmentRec tyName <$>
      (char '.' *> (FieldName <$> pName))
    pSeq =
      PathSegmentSeq tyName <$>
      between (char '[') (char ']') (intToIndex <$> L.decimal)
  pRec <|> pSeq
