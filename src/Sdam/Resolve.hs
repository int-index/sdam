{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving,
             ScopedTypeVariables, NamedFieldPuns #-}

module Sdam.Resolve
  ( Err(..),
    resolve
  ) where

import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Sdam.Core

data Err = MissingRefs (Set Ref)

resolve :: Ref -> Space -> Either Err Resolved
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

newtype Accum = Accum (Missing, Map Ref Resolved)
  deriving newtype (Semigroup, Monoid)

resolveObject ::
  Map Ref Resolved ->
  Map Ref (Object Ref) ->
  Ref ->
  Writer Accum Resolved
resolveObject resolvedSpace unresolvedSpace = go (Visited Set.empty) []
  where
    go :: Visited -> [PathSegment] -> Ref -> Writer Accum Resolved
    go Visited{visitedRefs} rpath ref =
      if Set.member ref visitedRefs
      then goVisited ref
      else goUnvisited Visited{visitedRefs} rpath ref

    goVisited :: Ref -> Writer Accum Resolved
    goVisited ref =
        -- (Map.!) is safe because if (Set.member ref visitedRefs), then
        -- we must have added the relevant object to 'resolvedSpace'.
        let resolved = resolvedSpace Map.! ref
        in (mempty, resolved{resLoop = True})

    goUnvisited ::
      Visited ->
      [PathSegment] ->
      Ref ->
      Writer Accum Resolved
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
              let visit i = go visited' (PathSegmentSeq (intToIndex i) : rpath)
              xs' <- Seq.traverseWithIndex visit xs
              retValue (ValueSeq xs')
            ValueRec fields -> do
              let visit fieldId = go visited' (PathSegmentRec fieldId : rpath)
              fields' <- Map.traverseWithKey visit fields
              retValue (ValueRec fields')
            ValueStr s -> do
              retValue (ValueStr s)

    reportMissing :: Ref -> Writer Accum Resolved
    reportMissing ref =
      let
        accum = Accum (Missing (Set.singleton ref), Map.empty)
        missingErr = error
          "resolveObject: contract violation, check for missing \
          \refs before forcing the result"
      in
        (accum, missingErr)
