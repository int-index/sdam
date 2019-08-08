{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving,
             NamedFieldPuns, StandaloneDeriving, TypeApplications,
             DeriveFunctor, DeriveFoldable, DeriveTraversable,
             DeriveGeneric #-}

module Sdam.Core
  (
    -- * Names
    TyName(..),
    tyNameStr,
    FieldName(..),
    fieldNameStr,

    -- * Types
    Schema(..),
    TyDefn(..),
    TyInst(..),
    TyUnion(..),
    tyUnionSingleton,
    tyUnionSequence,
    tyUnionRecursiveSequence,
    BrokenSchema(..),
    checkTyInst,

    -- * Values
    Value(..),

    -- * Paths
    Path(..),
    emptyPath,
    consPath,
    unconsPath,
    PathSegment(..),
    Index,
    intToIndex,
    indexToInt,
    PathBuilder(..),
    mkPathBuilder,
    buildPath,
    PathTrie(..),
    pathTrieLookup
  ) where

import Data.Hashable (Hashable)
import Data.HashMap.Strict as HashMap
import Data.HashSet as HashSet
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.String (IsString)
import Control.Exception (Exception, ArithException(Underflow), throw)
import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import Text.Regex.Applicative (RE)
import Data.Function (fix)

import Sdam.Name

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

newtype TyName = TyName Name
  deriving newtype (Eq, Ord, Show, IsString, Hashable)

tyNameStr :: TyName -> String
tyNameStr (TyName name) = nameToStr name

newtype FieldName = FieldName Name
  deriving newtype (Eq, Ord, Show, IsString, Hashable)

fieldNameStr :: FieldName -> String
fieldNameStr (FieldName name) = nameToStr name

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

{-

A 'Schema' defines the available syntactic constructs: records and strings.
For each record field there is a union of types that is allowed in this
position:

  "FnApp" => TyRec "fn"  => Var | Lam | Num | ...
                   "arg" => Var | Lam | Num | ...
  "Var"   => TyStr /regex/
  "Lam"   => ...
  "Num"   => ...

'fn' and 'arg' are record fields, and (Var | Lam | Num | ...) is a 'TyUnion'
that lists the syntactic constructs allowed in this position.

In practice, the 'TyUnion' is by necessity bigger than the actual set of
allowed constructs. There are various refinements imposed by the type system
of the language that is being described. For example, 'FnApp' shown above does
not disallow expressions like

  not 10

This makes the use of (Var | Lam | Num | ...) to describe 'fn' and 'arg'
incomplete - it allows us to construct invalid expressions.

As another example, when we consider strings that represent variable names, we
need to track the current scope to decide what strings are allowed.

Tracking scope, types, etc, is not always feasible or even possible, so we
treat any type union as a static approximation of the set of constructs allowed
in a given position.

A bit counter-intuitively, this means that the information contained in those
type unions is not the set of constructs that is allowed, but the set of
constructs that is forbidden (because not everything is included in this
union).

It is a static approximation in the following sense: a type union is the
tightest restriction that does not need to inspect other values. In other
words, the meta-language that we have here is not dependently-typed on purpose,
as it is not feasible to embed the scoping rules and the type system of the
language that is being described.

-}

data Schema =
  Schema
    { schemaTypes :: HashMap TyName TyDefn,
      schemaRoot :: TyUnion
    }

data TyDefn =
  TyDefnRec (HashSet FieldName) |
  TyDefnStr

data TyInst =
  -- {m:u,n:v}
  TyInstRec (HashMap FieldName TyUnion) |
  -- /regex/
  TyInstStr (RE Char ())

tyInstUnion :: TyName -> TyInst -> TyInst -> TyInst
tyInstUnion _ (TyInstStr re1) (TyInstStr re2) = TyInstStr (re1 <|> re2)
tyInstUnion _ (TyInstRec ks1) (TyInstRec ks2) = TyInstRec (HashMap.unionWith (<>) ks1 ks2)
tyInstUnion tyName _ _ = throw (BrokenSchemaInstSemigroup tyName)

-- A{m:u,n:v} | B/regex/ | (...)*
data TyUnion =
  TyUnion
    (HashMap TyName TyInst)   -- the types
    (Maybe TyUnion)           -- the (...)* clause
  --
  -- No Show/Eq/Hashable/etc instances as it is a common use case to have an
  -- infinite TyUnion (see tyUnionRecursiveSequence).
  --

instance Semigroup TyUnion where
  TyUnion u1 r1 <> TyUnion u2 r2 =
    TyUnion (HashMap.unionWithKey tyInstUnion u1 u2) (r1 <> r2)

instance Monoid TyUnion where
  mempty = TyUnion mempty mempty

tyUnionSingleton :: TyName -> TyInst -> TyUnion
tyUnionSingleton name inst = TyUnion (HashMap.singleton name inst) Nothing

tyUnionSequence :: TyUnion -> TyUnion
tyUnionSequence u = TyUnion HashMap.empty (Just u)

-- Arbitrarily nested elements.
-- Note that this gives rise to an infinite TyUnion:
--   A | B | ( A | B | ( A | B | ...)* )*
tyUnionRecursiveSequence :: TyUnion -> TyUnion
tyUnionRecursiveSequence u = fix (\r -> u <> tyUnionSequence r)

data BrokenSchema =
  BrokenSchemaInstNotStr TyName |
  BrokenSchemaInstNotRec TyName |
  BrokenSchemaInstBadKeys TyName (HashSet FieldName) |
  BrokenSchemaInstSemigroup TyName
  deriving (Eq, Show)

instance Exception BrokenSchema

checkTyInst :: TyName -> TyDefn -> TyInst -> TyInst
checkTyInst tyName tyDefn tyInst =
  case mExc of
    Nothing -> tyInst
    Just e -> throw e
  where
    mExc =
      case tyDefn of
        TyDefnStr ->
          case tyInst of
            TyInstRec _ -> Just (BrokenSchemaInstNotStr tyName)
            TyInstStr _ -> Nothing
        TyDefnRec defnKeys ->
          case tyInst of
            TyInstStr _ -> Just (BrokenSchemaInstNotRec tyName)
            TyInstRec instFields ->
              let instKeys = HashMap.keysSet instFields in
              if instKeys /= defnKeys
              then Just (BrokenSchemaInstBadKeys tyName instKeys)
              else Nothing

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

{-

'Value' is parametrized by the type of its fields. In the trivial case, we can
take the fixpoint of 'Value' to have values that are made of values:

  newtype AST = AST (Value AST)

However, we may also use this for extension:

  data Editable =
      Node UUID (Value Editable)
    | Hole

For a given 'Schema', there is a functional dependency between the 'TyName'
in the 'Value' and the shape of the 'Value':

  lookup(tyName, schema) is TyStr   ==>   value is ValueStr
  lookup(tyName, schema) is TyRec   ==>   value is ValueRec

A type-indexed representation could be used to guarantee this at compile-time,
but this would involve promoting the schema to the type-level and using
singleton types.

-}

data Value a =
  ValueRec TyName (HashMap FieldName a) |
  ValueStr TyName Text |
  ValueSeq (Seq a)
  deriving stock Show
  deriving stock (Functor, Foldable, Traversable)

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

newtype Path = Path [PathSegment]
  deriving newtype (Eq, Show)

emptyPath :: Path
emptyPath = Path []

consPath :: PathSegment -> Path -> Path
consPath ps (Path p) = Path (ps:p)

unconsPath :: Path -> Maybe (PathSegment, Path)
unconsPath (Path p) =
  case p of
    [] -> Nothing
    ps : p' -> Just (ps, Path p')

{-

Note that 'PathSegmentRec' is qualified by a 'TyName':

* This guarantees that there is no clash between fields with the same name
  across different types (that is, each type has its own namespace for fields).

* This safeguards against duck typing. We wouldn't want code that abstracts over
  values by what fields /names/ they have: abstraction should be over meaning,
  not over strings.

-}
data PathSegment =
  PathSegmentRec TyName FieldName |
  PathSegmentSeq Index
  deriving stock (Eq, Show, Generic)

instance Hashable PathSegment

-- Invariant: non-negative.
newtype Index = Index Int
  deriving newtype (Eq, Show, Hashable)

intToIndex :: Int -> Index
intToIndex i =
  if i < 0
    then throw Underflow
    else Index i

indexToInt :: Index -> Int
indexToInt (Index i) = i

newtype PathBuilder = PathBuilder (Path -> Path)

instance Semigroup PathBuilder where
  PathBuilder f <> PathBuilder g = PathBuilder (f . g)

instance Monoid PathBuilder where
  mempty = PathBuilder id

mkPathBuilder :: PathSegment -> PathBuilder
mkPathBuilder ps = PathBuilder (consPath ps)

buildPath :: PathBuilder -> Path
buildPath (PathBuilder pb) = pb emptyPath

data PathTrie a =
  PathTrie
    { pathTrieRoot :: a,
      pathTrieChildren :: HashMap PathSegment (PathTrie a)
    }

instance Semigroup a => Semigroup (PathTrie a) where
  PathTrie r1 c1 <> PathTrie r2 c2 =
    PathTrie (r1 <> r2) (HashMap.unionWith (<>) c1 c2)

instance Monoid a => Monoid (PathTrie a) where
  mempty = PathTrie mempty HashMap.empty

pathTrieLookup :: Monoid a => PathSegment -> PathTrie a -> PathTrie a
pathTrieLookup pathSegment pathTrie =
  HashMap.lookupDefault mempty pathSegment (pathTrieChildren pathTrie)
