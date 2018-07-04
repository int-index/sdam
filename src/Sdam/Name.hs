{-# LANGUAGE DerivingStrategies, NamedFieldPuns, TypeApplications,
             GeneralizedNewtypeDeriving #-}

module Sdam.Name
  ( Letter,
    unsafeCharToLetter,
    charToLetter,
    letterToChar,
    Name(..),
    nameToStr,
    strToName
  ) where

import Data.String (IsString(fromString))
import Data.Foldable (toList)
import Data.Char (isLetter)
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Control.Exception (Exception, throw)

-- Invariant: (Char.isLetter c)
newtype Letter = Letter Char
  deriving newtype (Eq, Ord, Show)

-- Precondition (unchecked): the input character satisfies 'Char.isLetter'.
unsafeCharToLetter :: Char -> Letter
unsafeCharToLetter = Letter

charToLetter :: Char -> Maybe Letter
charToLetter c
  | isLetter c = Just (Letter c)
  | otherwise = Nothing

letterToChar :: Letter -> Char
letterToChar (Letter c) = c

newtype Name = Name { nameParts :: NonEmpty (NonEmpty Letter) }
  deriving newtype (Eq, Ord)

nameToStr :: Name -> String
nameToStr = merge . inter . coerce @Name @(NonEmpty (NonEmpty Char))
  where
    merge = foldMap @[] (toList @NonEmpty) . toList @NonEmpty
    inter = NonEmpty.intersperse ('-' :| [])

strToName :: String -> Maybe Name
strToName = go [] []
  where
    go parts acc [] = do
      part <- fromAcc acc
      let nameParts = NonEmpty.reverse (part :| parts)
      Just Name{nameParts}
    go parts acc ('-':cs) = do
      part <- fromAcc acc
      go (part : parts) [] cs
    go parts acc (c:cs) =
      case charToLetter c of
        Nothing -> Nothing
        Just l -> go parts (l:acc) cs
    fromAcc =
      fmap NonEmpty.reverse . nonEmpty

data InvalidName = InvalidName String
  deriving stock Show

instance Exception InvalidName

instance IsString Name where
  fromString s =
    case strToName s of
      Nothing -> throw (InvalidName s)
      Just name -> name

instance Show Name where
  showsPrec d name = showsPrec d (nameToStr name)
