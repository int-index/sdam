{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving,
             ScopedTypeVariables, NamedFieldPuns #-}

module Sdam.Resolve
  ( Resolved(..),
    Err(..),
    resolve
  ) where

import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Sdam.Core

data Resolved p =
  Resolved
    { resRef :: Ref,
      resPath :: Path,
      resObject :: Object p (Resolved p),
      resLoop :: Bool }

data Err = MissingRefs (Set Ref)

resolve :: Ref -> Space p -> Either Err (Resolved p)
resolve headRef space = do
  let
    unresolvedSpace = spaceMap space
    (Accum (Missing{missingRefs}, resolvedSpace), resolvedHead) =
      resolveObject resolvedSpace unresolvedSpace headRef
  if Set.null missingRefs
    then Right resolvedHead
    else Left (MissingRefs missingRefs)

newtype Visited = Visited { visitedRefs :: Set Ref }

newtype Missing = Missing { missingRefs :: Set Ref }
  deriving newtype (Semigroup, Monoid)

type Writer = (,)

newtype Accum p = Accum (Missing, Map Ref (Resolved p))
  deriving newtype (Semigroup, Monoid)

resolveObject ::
  forall p.
  Map Ref (Resolved p) ->
  Map Ref (Object p Ref) ->
  Ref ->
  Writer (Accum p) (Resolved p)
resolveObject resolvedSpace unresolvedSpace = go (Visited Set.empty) []
  where
    go :: Visited -> [PathSegment] -> Ref -> Writer (Accum p) (Resolved p)
    go Visited{visitedRefs} rpath ref =
      if Set.member ref visitedRefs
      then goVisited ref
      else goUnvisited Visited{visitedRefs} rpath ref

    goVisited :: Ref -> Writer (Accum p) (Resolved p)
    goVisited ref =
        -- (Map.!) is safe because if (Set.member ref visitedRefs), then
        -- we must have added the relevant object to 'resolvedSpace'.
        let resolved = resolvedSpace Map.! ref
        in (mempty, resolved{resLoop = True})

    goUnvisited ::
      Visited ->
      [PathSegment] ->
      Ref ->
      Writer (Accum p) (Resolved p)
    goUnvisited Visited{visitedRefs} rpath ref =
      case Map.lookup ref unresolvedSpace of
        Nothing -> reportMissing ref
        Just (Object tyId value) -> do
          let
            mkResolved v =
              Resolved
                { resRef = ref,
                  resPath = Path (reverse rpath),
                  resObject = Object tyId v,
                  resLoop = False }
            mkAccum resolved@Resolved{resRef} =
              Accum (Missing Set.empty, Map.singleton resRef resolved)
            retValue v =
              let resolved = mkResolved v
              in (mkAccum resolved, resolved)
            visited' = Visited (Set.insert ref visitedRefs)
          case value of
            ValueSeq xs -> do
              let visit i = go visited' (PathSegmentSeq i : rpath)
              xs' <- traverseWithIndex visit xs
              retValue (ValueSeq xs')
            ValueRec fields -> do
              let visit fieldId = go visited' (PathSegmentRec fieldId : rpath)
              fields' <- Map.traverseWithKey visit fields
              retValue (ValueRec fields')
            ValuePrim p -> do
              retValue (ValuePrim p)

    reportMissing :: Ref -> Writer (Accum p) (Resolved p)
    reportMissing ref =
      let
        accum = Accum (Missing (Set.singleton ref), Map.empty)
        missingErr = error
          "resolveObject: contract violation, check for missing \
          \refs before forcing the result"
      in
        (accum, missingErr)

traverseWithIndex :: Applicative f => (Index -> a -> f b) -> [a] -> f [b]
traverseWithIndex f xs =
  let f' i x = f (Index i) x
  in sequenceA $ zipWith f' [0..] xs
