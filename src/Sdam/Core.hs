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
    Ty(..),
    TyUnion(..),

    -- * Values
    Object(..),
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
import Data.HashSet (HashSet)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.String (IsString)
import Control.Exception (ArithException(Underflow), throw)
import GHC.Generics (Generic)

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

A 'Schema defines the available syntactic constructs: records, sequences, and
strings. In each subtree position (record fields and sequence elements) there
is a union of types that are allowed in this position. For instance:

  FnApp = fn, arg: <expr>;

'fn' and 'arg' are subtree positions (record fields, in this case), and
<expr> is a metavariable that defines a 'TyUnion' that describes syntactic
constructs that can be used in these positions.

This union is by necessity bigger than the actual set of allowed constructs.
There are various refinements imposed by the type system of the language that
is being described. For example, 'FnApp' shown above does not disallow
expressions like

  negate "hello"

This makes the use of <expr> to describe 'fn' and 'arg' incomplete - it allows
us to construct invalid expressions.

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

newtype Schema = Schema { schemaTypes :: HashMap TyName Ty }
  deriving newtype Show

data Ty =
  TyRec (HashMap FieldName TyUnion) |
  TySeq TyUnion |
  TyStr
  deriving stock Show

newtype TyUnion = TyUnion (HashSet TyName)
  deriving newtype (Show, Semigroup, Monoid, Eq, Hashable)

--------------------------------------------------------------------------------
-- Objects/Values
--------------------------------------------------------------------------------

{-

'Object' and 'Value' are parametrized by the type of their fields. In the trivial
case, we can take the fixpoint of 'Object' to have objects that are made of objects:

  newtype AST = AST (Object AST)

However, we may also use this for extension:

  data Editable =
      Node UUID (Object Editable)
    | Hole

For a given 'Schema', there is a functional dependency between the 'TyName'
in the 'Object' and the shape of the 'Value':

  lookup(tyName, schema) is TyStr   ==>   value is ValueStr
  lookup(tyName, schema) is TySeq   ==>   value is ValueSeq
  lookup(tyName, schema) is TyRec   ==>   value is ValueRec

A type-indexed representation could be used to guarantee this at compile-time,
but this would involve promoting the schema to the type-level and using
singleton types.

-}
data Object a = Object TyName (Value a)
  deriving stock Show
  deriving stock (Functor, Foldable, Traversable)

data Value a =
  ValueRec (HashMap FieldName a) |
  ValueSeq (Seq a) |
  ValueStr Text
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

Note that 'PathSegment' is qualified by a 'TyName':

* This guarantees that there is no clash between fields with the same name
  across different types (that is, each type has its own namespace for fields).

* This safeguards against duck typing. We wouldn't want code that abstracts over
  values by what fields /names/ they have: abstraction should be over meaning,
  not over strings.

-}
data PathSegment =
  PathSegmentRec TyName FieldName |
  PathSegmentSeq TyName Index
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
