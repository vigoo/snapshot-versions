{-# LANGUAGE RecordWildCards #-}

module SnapshotVersions.ProcessedPackages where

import           Control.Applicative
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set            as Set

data DependentPackage =
  DependentPackage { dpName            :: String
                   , dpExplicitVersion :: Maybe String
                   }
  deriving (Eq, Ord)

instance Show DependentPackage where
  show (DependentPackage dpName (Just ver)) = dpName <> "-" <> ver
  show (DependentPackage dpName Nothing) = dpName

data ProcessedPackages = ProcessedPackages { ppMap :: Map.Map String DependentPackage }

asProcessedPackages :: Set.Set DependentPackage -> ProcessedPackages
asProcessedPackages =
  ProcessedPackages . Set.foldr (\dp m -> Map.insert (dpName dp) dp m) Map.empty

addProcessedPackage :: DependentPackage -> ProcessedPackages -> ProcessedPackages
addProcessedPackage pkg pps =
  mergeProcessedPackages [pps, asProcessedPackages (Set.singleton pkg)]

findNewPackages :: ProcessedPackages -> Set.Set DependentPackage -> ProcessedPackages
findNewPackages pp@(ProcessedPackages ppMap) dps =
  asProcessedPackages $ Set.filter (\dp -> (not (member dp)) || (addsExplicitVersion dp pp)) dps

  where
    member dp = Map.member (dpName dp) ppMap

mergeProcessedPackages :: [ProcessedPackages] -> ProcessedPackages
mergeProcessedPackages = foldr mergeTwoSets (ProcessedPackages Map.empty)
  where
    mergeTwoSets :: ProcessedPackages -> ProcessedPackages -> ProcessedPackages
    mergeTwoSets (ProcessedPackages sourceMap) (ProcessedPackages targetMap) =
      ProcessedPackages $ Map.foldr mergeSinglePackage targetMap sourceMap

    mergeSinglePackage :: DependentPackage -> Map.Map String DependentPackage -> Map.Map String DependentPackage
    mergeSinglePackage (dp@(DependentPackage{..})) res =
      Map.alter (alterPackageEntry dp) dpName res

    alterPackageEntry :: DependentPackage -> Maybe DependentPackage -> Maybe DependentPackage
    alterPackageEntry dp Nothing = Just dp
    alterPackageEntry dp1 (Just dp0) =
      case dpExplicitVersion dp0 of
        Just ver -> Just dp0
        Nothing -> Just dp1

isProcessed :: DependentPackage -> ProcessedPackages -> Bool
isProcessed pkg = Map.member (dpName pkg) . ppMap

addsExplicitVersion :: DependentPackage -> ProcessedPackages -> Bool
addsExplicitVersion dp (ProcessedPackages ppMap) =
  case Map.lookup (dpName dp) ppMap of
    Just (DependentPackage _ Nothing) -> isJust (dpExplicitVersion dp)
    _ -> False

toList :: ProcessedPackages -> [DependentPackage]
toList = Map.elems . ppMap
