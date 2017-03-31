module Ingredients.Mash
(
  Mash(..)
, mkMash
, addFermentable
, MashStep(..)
, MashProfile(..)
, MashStepType(..)
)
where
import Ingredients.Common
import Ingredients.Fermentable
import Ingredients.Water
import Data.List.NonEmpty

data Mash = AddWater (NonEmpty Water)
    | AddFermentable Fermentable Mash deriving (Show, Eq)
--    | DoMash InitialTemperature FinalTemperature Duration Mash deriving (Show)

    
mkMash :: NonEmpty Water -> Mash
mkMash vol = AddWater vol

addFermentable :: Fermentable -> Mash -> Mash
addFermentable ferm mash = AddFermentable ferm mash

data MashStepType = Infusion Volume | Temperature | Decoction deriving (Eq, Show)

data MashStep = MashStep
            {
                stepName :: String,
                stepVersion :: Int,
                stepType :: MashStepType,
                stepTemp :: Temperature,
                stepTime :: Duration,
                rampTime :: Maybe Duration,
                endTemp :: Maybe Temperature
            } | TestMashStep String
    deriving (Eq, Show)

data MashProfile = MashProfile
            {
                profileName :: String,
                profileVersion :: Int,
                grainTemp :: Temperature,
                mashSteps :: NonEmpty MashStep,
                notes :: Maybe String
                
            } | TestMashProfile String
    deriving (Eq, Show)
