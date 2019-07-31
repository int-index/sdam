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
import Data.Sequence.NonEmpty (NonEmptySeq(..))
import Data.Text (Text)
import Data.Maybe
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Sequence as Seq
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
  ValueRec <$> pValueRec

buildHashMapSeq :: (Eq k, Hashable k) => [(k, v)] -> HashMap k (NonEmptySeq v)
buildHashMapSeq =
    HashMap.map (\f -> mkNonEmptySeq (f [])) .
    HashMap.fromListWith (.) .
    map (\(k, v) -> (k, (v:)))  -- to DList
  where
    mkNonEmptySeq (x:xs) = NonEmptySeq x (Seq.fromList xs)
    mkNonEmptySeq [] = error "buildHashMapSeq: impossible, empty list"

pValueRec :: Parser ObjectParseErr (HashMap FieldName (NonEmptySeq ParsedObject))
pValueRec =
  between (pSymbol "{") (pSymbol "}") $
  buildHashMapSeq <$> (pFieldObject `sepBy` pComma)

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

pFieldObject :: Parser ObjectParseErr (FieldName, ParsedObject)
pFieldObject = do
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
  _ <- char '.'
  fieldName <- FieldName <$> pName
  i <- between (char '[') (char ']') (intToIndex <$> L.decimal)
  return (PathSegment tyName fieldName i)
