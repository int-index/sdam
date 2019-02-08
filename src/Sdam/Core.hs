{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving,
             NamedFieldPuns, StandaloneDeriving, TypeApplications,
             DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Sdam.Core
  (
    -- * Names
    TyName(..),
    tyNameStr,
    FieldName(..),
    fieldNameStr,

    -- * Types
    Env(..),
    Ty(..),
    TyUnion(..),

    -- * References
    TyId(..),
    mkTyId,
    tyIdToStr,
    strToTyId,
    FieldId(..),
    mkFieldId,
    fieldIdToStr,
    strToFieldId,
    Ref(..),
    refToStr,
    strToRef,

    -- * Values
    Space(..),
    spaceRoots,
    Object(..),
    Value(..),
    Resolved(..),

    -- * Paths
    Path(..),
    PathSegment(..),
    Index,
    intToIndex,
    indexToInt,
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.Foldable (toList)
import Data.String (IsString(fromString))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Exception (Exception, ArithException(Underflow), throw)

import Sdam.Name
import Sdam.Fingerprint

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

newtype TyName = TyName { tyName :: Name }
  deriving newtype (Eq, Ord, Show, IsString)

tyNameStr :: TyName -> String
tyNameStr TyName{tyName} = nameToStr tyName

newtype FieldName = FieldName { fieldName :: Name }
  deriving newtype (Eq, Ord, Show, IsString)

fieldNameStr :: FieldName -> String
fieldNameStr FieldName{fieldName} = nameToStr fieldName

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

{-

An 'Env' defines the available syntactic constructs: records, sequences, and
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

newtype Env = Env { envMap :: Map TyName Ty }
  deriving newtype Show

data Ty =
  TyRec (Map FieldName TyUnion) |
  TySeq TyUnion |
  TyStr
  deriving stock Show

data TyUnion = TyUnion (Set TyName)
  deriving stock Show

instance Semigroup TyUnion where
  TyUnion tns1 <> TyUnion tns2 = TyUnion (Set.union tns1 tns2)

instance Monoid TyUnion where
  mempty = TyUnion Set.empty

--------------------------------------------------------------------------------
-- References
--------------------------------------------------------------------------------

{-

We define 'TyId' and 'FieldId' as fingerprints rather than strings:

* We get fast comparisons this way. Comparing two Word64 is far better than
  comparing entire lists of characters.

* Name length becomes irrelevant. We wouldn't want longer names to make
  serialized values to take more space or anything like that.

* Unfortunately, this means we can't pretty-print a 'Value' without consulting
  an 'Env' because hashing is not invertible. See the Sdam.NameInfo module.

-}

newtype TyId = TyId { tyIdFingerprint :: Fingerprint }
  deriving newtype (Eq, Ord)

mkTyId :: TyName -> TyId
mkTyId tyName = TyId (fingerprintString (tyNameStr tyName))

data InvalidTyIdStr = InvalidTyIdStr String
  deriving stock Show

instance Exception InvalidTyIdStr

instance IsString TyId where
  fromString s =
    case strToTyId s of
      Nothing -> throw (InvalidTyIdStr s)
      Just tyId -> tyId

instance Show TyId where
  showsPrec d tyId = showsPrec d (tyIdToStr tyId)

tyIdToStr :: TyId -> String
tyIdToStr = fingerprintToString . tyIdFingerprint

strToTyId :: String -> Maybe TyId
strToTyId = fmap TyId . stringToFingerprint

newtype FieldId = FieldId { fieldIdFingerprint :: Fingerprint }
  deriving newtype (Eq, Ord)

{-

Note that 'FieldId' is made from both 'TyName' and 'FieldName':

* This guarantees that there is no clash between fields with the same name
  across different types (that is, each type has its own namespace for fields),
  for example in paths.

* This safeguards against duck typing. We wouldn't want code that abstracts over
  values by what fields /names/ they have: abstraction should be over meaning,
  not over strings.

-}

mkFieldId :: TyName -> FieldName -> FieldId
mkFieldId ty fieldName =
  let
    TyId tyFp = mkTyId ty
    fldFp = fingerprintString (fieldNameStr fieldName)
  in
    FieldId (fingerprintFingerprints [tyFp, fldFp])

data InvalidFieldIdStr = InvalidFieldIdStr String
  deriving stock Show

instance Exception InvalidFieldIdStr

instance IsString FieldId where
  fromString s =
    case strToFieldId s of
      Nothing -> throw (InvalidFieldIdStr s)
      Just fieldId -> fieldId

instance Show FieldId where
  showsPrec d fieldId = showsPrec d (fieldIdToStr fieldId)

fieldIdToStr :: FieldId -> String
fieldIdToStr = fingerprintToString . fieldIdFingerprint

strToFieldId :: String -> Maybe FieldId
strToFieldId = fmap FieldId . stringToFingerprint

-- References can be generated by incrementing a counter, generating UUIDs,
-- hashing strings, etc. The only requirement is that new references are
-- distinct from the previous ones within a single 'Space'.
newtype Ref = Ref { refFingerprint :: Fingerprint }
  deriving newtype (Eq, Ord)

data InvalidRefStr = InvalidRefStr String
  deriving stock Show

instance Exception InvalidRefStr

instance IsString Ref where
  fromString s =
    case strToRef s of
      Nothing -> throw (InvalidRefStr s)
      Just ref -> ref

instance Show Ref where
  showsPrec d ref = showsPrec d (refToStr ref)

refToStr :: Ref -> String
refToStr = fingerprintToString . refFingerprint

strToRef :: String -> Maybe Ref
strToRef = fmap Ref . stringToFingerprint

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

newtype Space =
  Space { spaceMap :: Map Ref (Object Ref) }
  deriving newtype Show

spaceRoots :: Space -> Set Ref
spaceRoots Space{spaceMap} =
  Map.keysSet spaceMap Set.\\
  foldMap @(Map _) (Set.fromList . toList @Object) spaceMap

data Object a = Object TyId (Value a)
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance Show a => Show (Object a)

data Value a =
  ValueRec (Map FieldId a) |
  ValueSeq (Seq a) |
  ValueStr String
  deriving stock Show
  deriving stock (Functor, Foldable, Traversable)

data Resolved =
  Resolved
    { resRef :: Ref,
      resPath :: Path,
      resObject :: Object Resolved,
      resLoop :: Bool }

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

newtype Path = Path [PathSegment]

data PathSegment =
  PathSegmentRec FieldId |
  PathSegmentSeq Index

-- Invariant: non-negative.
newtype Index = Index Int

intToIndex :: Int -> Index
intToIndex i =
  if i < 0
    then throw Underflow
    else Index i

indexToInt :: Index -> Int
indexToInt (Index i) = i
