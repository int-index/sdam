{-# LANGUAGE LambdaCase, DerivingStrategies, RecordWildCards,
             GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Sdam.Parser
  (
    -- MetaVar
    MetaVar(..),
    metaVarStr,

    -- Env
    pEnv,
    EnvParseErr(..),

    -- Object/Value
    pObject,
    ParsedObject(..),

    -- Path
    pPath,

    -- Running
    parse
  ) where

import Data.Hashable (Hashable)
import Control.Monad
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Graph
import Data.HashMap.Strict (HashMap)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Semigroup
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Exts (IsList(fromList))
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.NonEmpty as NonEmpty

import Sdam.Name
import Sdam.Core

--------------------------------------------------------------------------------
-- MetaVar
--------------------------------------------------------------------------------

newtype MetaVar = MetaVar { metaVarName :: Name }
  deriving newtype (Eq, Ord, Hashable)

metaVarStr :: MetaVar -> String
metaVarStr MetaVar{metaVarName} = nameToStr metaVarName

data Ty' =
  TyRec' [(FieldName, TyU)] |
  TySeq' TyU |
  TyStr'

data TyU = TyU (Set MetaVar) (Set TyName)

tyU_metaVars :: TyU -> Set MetaVar
tyU_metaVars (TyU mv _) = mv

instance Semigroup TyU where
  TyU mv1 tn1 <> TyU mv2 tn2 =
    TyU (Set.union mv1 mv2) (Set.union tn1 tn2)

instance Monoid TyU where
  mempty = TyU Set.empty Set.empty

tyU_MetaVar :: MetaVar -> TyU
tyU_MetaVar mv = TyU (Set.singleton mv) Set.empty

tyU_TyName :: TyName -> TyU
tyU_TyName tn = TyU Set.empty (Set.singleton tn)

--------------------------------------------------------------------------------
-- Common
--------------------------------------------------------------------------------

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

pColon :: Ord e => Parser e ()
pColon = pSymbol ":"

pSemicolon :: Ord e => Parser e ()
pSemicolon = pSymbol ";"

pComma :: Ord e => Parser e ()
pComma = pSymbol ","

pBar :: Ord e => Parser e ()
pBar = pSymbol "|"

pLetter :: Ord e => Parser e Letter
pLetter = unsafeCharToLetter <$> letterChar

pName :: Ord e => Parser e Name
pName = Name <$> (NonEmpty.some pLetter `NonEmpty.sepBy1` char '-')

pTyName :: Ord e => Parser e TyName
pTyName = pLexeme (TyName <$> pName)

pFieldName :: Ord e => Parser e FieldName
pFieldName = pLexeme (FieldName <$> pName)

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------

data EnvParseErr =
  EnvConflictingTypeDecls [TyName] |
  EnvConflictingFieldDecls TyName [FieldName] |
  EnvConflictingMetaDecls [MetaVar] |
  EnvCyclicMetaDecls [MetaVar] |
  EnvMetaVarNotDefined MetaVar
  deriving stock (Eq, Ord)

instance ShowErrorComponent EnvParseErr where
  showErrorComponent = \case
    EnvConflictingTypeDecls tyNames ->
      "conflicting type declarations: " ++
      concat (intersperse ", " (map tyNameStr tyNames))
    EnvConflictingFieldDecls tyName fieldNames ->
      "conflicting field names in " ++ tyNameStr tyName ++ ": " ++
      concat (intersperse ", " (map fieldNameStr fieldNames))
    EnvConflictingMetaDecls metaVars ->
      "conflicting metavariable declarationss: " ++
      concat (intersperse ", " (map metaVarStr metaVars))
    EnvCyclicMetaDecls metaVars ->
      "cyclic metavariable definitions: " ++
      concat (intersperse ", " (map metaVarStr metaVars))
    EnvMetaVarNotDefined metaVar ->
      "undefined metavariable: " ++ metaVarStr metaVar

data Decl = TyDecl (TyName, Ty') | MetaDecl (MetaVar, TyU)

partitionDecls :: [Decl] -> ([(TyName, Ty')], [(MetaVar, TyU)])
partitionDecls = go [] []
  where
    go tyDecls metaDecls [] = (tyDecls, metaDecls)
    go tyDecls metaDecls (x:xs) =
      case x of
        TyDecl a -> go (a:tyDecls) metaDecls xs
        MetaDecl a -> go tyDecls (a:metaDecls) xs

pEnv :: Parser EnvParseErr Env
pEnv = do
  -- The parser operates in three stages:
  --   * parse declarations
  --   * process metavariables
  --   * process type declarations
  --
  -- In the input, metavariables are sets of type names and other metavariables,
  -- like this:
  --
  -- <m> = A | B | <n>
  -- <n> = C | D
  --
  -- We toposort them and perform normalization, so we end up with metavariables
  -- that stand for sets of type names (and no other metavariables).
  --
  -- <m> = A | B | C | D
  -- <n> = C | D
  --
  -- And then we substitute metavariable definitions in type declarations.
  decls <- many pDecl
  let (tyDecls', metaDecls') = partitionDecls decls
  metaDecls <- substMetaDecls metaDecls'
  tyDecls <- substTyDecls metaDecls tyDecls'
  return Env{ envMap = tyDecls }

substMetaDecls ::
  [(MetaVar, TyU)] ->
  Parser EnvParseErr (HashMap MetaVar TyUnion)
substMetaDecls metaVars' = do
  let dups = getDups (map fst metaVars')
  unless (null dups) $ customFailure (EnvConflictingMetaDecls dups)
  let
    mkMetaNode (metaVar, tyU) = (tyU, metaVar, Set.toList (tyU_metaVars tyU))
    metaVarGroups = stronglyConnCompR (map mkMetaNode metaVars')
  metaVarsNoLoops <- forM metaVarGroups $ \case
    AcyclicSCC (tyU, metaVar, _) -> return (metaVar, tyU)
    CyclicSCC grp -> customFailure (EnvCyclicMetaDecls (map snd3 grp))
  let
    go acc [] = return acc
    go acc ((metaVar, tyU):xs) = do
      -- any metavariable used in 'tyU' must be present in 'acc'
      -- because the input list is toposorted
      tyUnion <- substTyU acc tyU
      go (HashMap.insert metaVar tyUnion acc) xs
  go HashMap.empty metaVarsNoLoops

lookupMetaVar :: HashMap MetaVar TyUnion -> MetaVar -> Parser EnvParseErr TyUnion
lookupMetaVar metaDecls mv =
  case HashMap.lookup mv metaDecls of
    Nothing -> customFailure (EnvMetaVarNotDefined mv)
    Just a -> return a

substTyU :: HashMap MetaVar TyUnion -> TyU -> Parser EnvParseErr TyUnion
substTyU metaDecls (TyU mvs tns) = do
  tyUnions' <- traverse (lookupMetaVar metaDecls) (Set.toList mvs)
  return $ sconcat (TyUnion tns :| tyUnions')

substTyDecls ::
  HashMap MetaVar TyUnion ->
  [(TyName, Ty')] ->
  Parser EnvParseErr (HashMap TyName Ty)
substTyDecls metaDecls tyDecls' = do
  let dups = getDups (map fst tyDecls')
  unless (null dups) $ customFailure (EnvConflictingTypeDecls dups)
  tyDecls <- traverse (substTyDecl metaDecls) tyDecls'
  return (HashMap.fromList tyDecls)

substTyDecl ::
  HashMap MetaVar TyUnion ->
  (TyName, Ty') ->
  Parser EnvParseErr (TyName, Ty)
substTyDecl metaDecls (tyName, ty') =
  case ty' of
    TySeq' tyU -> do
      tyUnion <- substTyU metaDecls tyU
      return (tyName, TySeq tyUnion)
    TyRec' fields' -> do
      let dups = getDups (map fst fields')
      unless (null dups) $ customFailure (EnvConflictingFieldDecls tyName dups)
      fields <-
        forM fields' $ \(fieldName, tyU) -> do
          tyUnion <- substTyU metaDecls tyU
          return (fieldName, tyUnion)
      return (tyName, TyRec (HashMap.fromList fields))
    TyStr' ->
      return (tyName, TyStr)

pDecl :: Parser EnvParseErr Decl
pDecl = TyDecl <$> pTyDecl <|> MetaDecl <$> pMetaDecl

pMetaDecl :: Parser EnvParseErr (MetaVar, TyU)
pMetaDecl = do
  metaVar <- pMetaVar
  pSymbol "="
  tyU <- pTyU
  pSemicolon
  return (metaVar, tyU)

pTyU1 :: Parser EnvParseErr TyU
pTyU1 =
  tyU_MetaVar <$> pMetaVar <|>
  tyU_TyName <$> pTyName

pTyU :: Parser EnvParseErr TyU
pTyU = sconcat <$> (pTyU1 `NonEmpty.sepBy1` pBar)

pTyDecl :: Parser EnvParseErr (TyName, Ty')
pTyDecl = do
  tyName <- pTyName
  ty <-
    pSymbol "=" *> pTy <|>
    return (TyRec' [])
  pSemicolon
  return (tyName, ty)

pTy :: Parser EnvParseErr Ty'
pTy =
  TyStr' <$ pSymbol "!str" <|>
  TyRec' <$> try pRecTy <|>
  TySeq' <$> pSeqTy

pRecTy :: Parser EnvParseErr [(FieldName, TyU)]
pRecTy = concat <$> (pFieldDecl `sepBy1` pComma)

pFieldDecl :: Parser EnvParseErr [(FieldName, TyU)]
pFieldDecl = do
  fieldNames <- pFieldName `sepBy1` pComma
  pColon
  tyU <- pTyU
  return [(fieldName, tyU) | fieldName <- fieldNames]

pSeqTy :: Parser EnvParseErr TyU
pSeqTy =
  between (pSymbol "(") (pSymbol ")*") pTyU <|>
  pTyU1 <* pSymbol "*"

pMetaVar :: Parser EnvParseErr MetaVar
pMetaVar = pLexeme $ MetaVar <$> between (char '<') (char '>') pName

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

pValueStr :: Parser ObjectParseErr String
pValueStr = pLexeme $ do
  void (char '\"')
  catMaybes <$> manyTill pChar (char '\"')
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
  void (char '.')
  let
    pRec = PathSegmentRec tyName <$> (FieldName <$> pName)
    pSeq = PathSegmentSeq tyName <$> (intToIndex <$> L.decimal)
  pRec <|> pSeq

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

getDups :: Ord a => [a] -> [a]
getDups =
  Map.keys .
  Map.filter id .
  Map.fromListWith (\_ _ -> True) .
  map (\x -> (x,False))

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x
