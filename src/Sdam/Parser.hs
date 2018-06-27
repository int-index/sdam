{-# LANGUAGE LambdaCase, DerivingStrategies, RecordWildCards,
             GeneralizedNewtypeDeriving #-}

module Sdam.Parser where

import Control.Monad
import Data.List
import Data.Graph
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Sdam.Core
import Sdam.MetaVar

data Err =
  ConflictingTypeDecls [TyName] |
  ConflictingFieldDecls TyName [FieldName] |
  ConflictingMetaDecls [MetaVar] |
  CyclicMetaDecls [MetaVar] |
  MetaVarNotDefined MetaVar
  deriving stock (Eq, Ord)

instance ShowErrorComponent Err where
  showErrorComponent = \case
    ConflictingTypeDecls tyNames ->
      "conflicting type declarations: " ++
      concat (intersperse ", " (map tyNameStr tyNames))
    ConflictingFieldDecls tyName fieldNames ->
      "conflicting field names in " ++ tyNameStr tyName ++ ": " ++
      concat (intersperse ", " (map fieldNameStr fieldNames))
    ConflictingMetaDecls metaVars ->
      "conflicting metavariable declarationss: " ++
      concat (intersperse ", " (map metaVarStr metaVars))
    CyclicMetaDecls metaVars ->
      "cyclic metavariable definitions: " ++
      concat (intersperse ", " (map metaVarStr metaVars))
    MetaVarNotDefined metaVar ->
      "undefined metavariable: " ++ metaVarStr metaVar

type Parser = Parsec Err String

pSpace :: Parser ()
pSpace = L.space space1 pLineComment pBlockComment
  where
    pLineComment = L.skipLineComment "//"
    pBlockComment = L.skipBlockCommentNested "/*" "*/"

pLexeme :: Parser a -> Parser a
pLexeme = L.lexeme pSpace

pSymbol :: String -> Parser ()
pSymbol s = void (L.symbol pSpace s)

pColon :: Parser ()
pColon = pSymbol ":"

pSemicolon :: Parser ()
pSemicolon = pSymbol ";"

pComma :: Parser ()
pComma = pSymbol ","

pBar :: Parser ()
pBar = pSymbol "|"

data Decl = TyDecl (TyName, Ty') | MetaDecl (MetaVar, TyU)

partitionDecls :: [Decl] -> ([(TyName, Ty')], [(MetaVar, TyU)])
partitionDecls = go [] []
  where
    go tyDecls metaDecls [] = (tyDecls, metaDecls)
    go tyDecls metaDecls (x:xs) =
      case x of
        TyDecl a -> go (a:tyDecls) metaDecls xs
        MetaDecl a -> go tyDecls (a:metaDecls) xs

pEnv :: Parser Env
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
  Parser (Map MetaVar TyUnion)
substMetaDecls metaVars' = do
  let dups = getDups (map fst metaVars')
  unless (null dups) $ customFailure (ConflictingMetaDecls dups)
  let
    mkMetaNode (metaVar, tyU) = (tyU, metaVar, Set.toList (tyU_metaVars tyU))
    metaVarGroups = stronglyConnCompR (map mkMetaNode metaVars')
  metaVarsNoLoops <- forM metaVarGroups $ \case
    AcyclicSCC (tyU, metaVar, _) -> return (metaVar, tyU)
    CyclicSCC grp -> customFailure (CyclicMetaDecls (map snd3 grp))
  let
    go acc [] = return acc
    go acc ((metaVar, tyU):xs) = do
      -- any metavariable used in 'tyU' must be present in 'acc'
      -- because the input list is toposorted
      tyUnion <- substTyU acc tyU
      go (Map.insert metaVar tyUnion acc) xs
  go Map.empty metaVarsNoLoops

lookupMetaVar :: Map MetaVar TyUnion -> MetaVar -> Parser TyUnion
lookupMetaVar metaDecls mv =
  case Map.lookup mv metaDecls of
    Nothing -> customFailure (MetaVarNotDefined mv)
    Just a -> return a

substTyU :: Map MetaVar TyUnion -> TyU -> Parser TyUnion
substTyU metaDecls (TyU mvs tns) = do
  tyUnions' <- traverse (lookupMetaVar metaDecls) (Set.toList mvs)
  return $ mconcat (TyUnion tns : tyUnions')

substTyDecls ::
  Map MetaVar TyUnion ->
  [(TyName, Ty')] ->
  Parser (Map TyName Ty)
substTyDecls metaDecls tyDecls' = do
  let dups = getDups (map fst tyDecls')
  unless (null dups) $ customFailure (ConflictingTypeDecls dups)
  tyDecls <- traverse (substTyDecl metaDecls) tyDecls'
  return (Map.fromList tyDecls)

substTyDecl ::
  Map MetaVar TyUnion ->
  (TyName, Ty') ->
  Parser (TyName, Ty)
substTyDecl metaDecls (tyName, ty') =
  case ty' of
    TySeq' tyU -> do
      tyUnion <- substTyU metaDecls tyU
      return (tyName, TySeq tyUnion)
    TyRec' fields' -> do
      let dups = getDups (map fst fields')
      unless (null dups) $ customFailure (ConflictingFieldDecls tyName dups)
      fields <-
        forM fields' $ \(fieldName, tyU) -> do
          tyUnion <- substTyU metaDecls tyU
          return (fieldName, tyUnion)
      return (tyName, TyRec (Map.fromList fields))

pDecl :: Parser Decl
pDecl = TyDecl <$> pTyDecl <|> MetaDecl <$> pMetaDecl

pTyUnion :: Parser TyUnion
pTyUnion = TyUnion . Set.fromList <$> pTyName `sepBy` pBar

pLetters :: Parser String
pLetters = takeWhile1P (Just "letter") Char.isLetter

pTyName :: Parser TyName
pTyName = pLexeme (TyName <$> pLetters)

pFieldName :: Parser FieldName
pFieldName = pLexeme (FieldName <$> pLetters)

pMetaDecl :: Parser (MetaVar, TyU)
pMetaDecl = do
  metaVar <- pMetaVar
  pSymbol "="
  tyU <- pTyU
  pSemicolon
  return (metaVar, tyU)

pTyU1 :: Parser TyU
pTyU1 =
  tyU_MetaVar <$> pMetaVar <|>
  tyU_TyName <$> pTyName

pTyU :: Parser TyU
pTyU = mconcat <$> (pTyU1 `sepBy1` pBar)

pTyDecl :: Parser (TyName, Ty')
pTyDecl = do
  tyName <- pTyName
  ty <-
    pSymbol "=" *> pTy <|>
    return (TyRec' [])
  pSemicolon
  return (tyName, ty)

pTy :: Parser Ty'
pTy =
  TyRec' <$> try pRecTy <|>
  TySeq' <$> pSeqTy

pRecTy :: Parser [(FieldName, TyU)]
pRecTy = concat <$> (pField `sepBy1` pComma)

pField :: Parser [(FieldName, TyU)]
pField = do
  fieldNames <- pFieldName `sepBy1` pComma
  pColon
  tyU <- pTyU
  return [(fieldName, tyU) | fieldName <- fieldNames]

pSeqTy :: Parser TyU
pSeqTy =
  between (pSymbol "(") (pSymbol ")*") pTyU <|>
  pTyU1 <* pSymbol "*"

pMetaVar :: Parser MetaVar
pMetaVar = pLexeme $ MetaVar <$> between (char '<') (char '>') pLetters

getDups :: Ord a => [a] -> [a]
getDups =
  Map.keys .
  Map.filter id .
  Map.fromListWith (\_ _ -> True) .
  map (\x -> (x,False))

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

example :: String
example = unlines
  [ "Lam =",
    "  var: Str,",
    "  ty: <expr>,",
    "  body: <expr>;",
    "",
    "Var = name: Str, i: Nat;",
    "",
    "App = fn, arg: <expr>;",
    "",
    "Tup = <expr>*;",
    "",
    "<expr> = Lam | Pi | App | Var | Tup;" ]
