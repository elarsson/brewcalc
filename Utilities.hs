module Utilities
(-- Calibrate(..)
--, Dilute(..)
printMash
, boilOff
, getVolume
, estimateDensityAfterSparge
) where
import Ingredients

mkMash :: Volume -> Mash
mkMash vol = AddWater vol

printMash :: Mash -> String
printMash _ = "TJOHO!"

boilOff :: Duration -> Volume
boilOff (Minutes m) = Milliliters $ m * (4000.0 / 60.0) -- TODO: 4 liters/hour... don't hardcode

getVolume :: Wort -> Volume
getVolume (Sparge _ vol _) = vol
getVolume (AddHops _ _ w) = getVolume w
getVolume (Boil _ w) = getVolume w
getVolume (Chill _ w) = getVolume w
getVolume (Concentrate v _) = v
getVolume (Dilute v _) = v

calculateFG :: Wort -> Density
calculateFG _ = Density 1.05

estimateDensityAfterSparge :: Mash -> Density
estimateDensityAfterSparge _ = Density 1.05

-- data Wort = Sparge Mash ResultingVolume Density
    -- | AddHops Hops Weight Wort
    -- | Boil Duration Wort
    -- | Chill Duration FinalTemperature Wort
    -- | Concentrate ResultingVolume Wort
    -- | Dilute ResultingVolume Wort deriving (Show)
