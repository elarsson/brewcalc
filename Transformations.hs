module Transformations
( mkMash
, addFermentable
, sparge
, boil
, boilWithEvaporation
, chill
, spargeWithEstimate
, addHops
, ferment
--, boil
--, ferment
--, utilization
) where
import Ingredients
import Utilities

mkMash :: Volume -> Mash
mkMash vol = AddWater vol

addFermentable :: Fermentable -> Weight -> Mash -> Mash
addFermentable ferm wt mash = AddGrain ferm wt mash

doMash :: Mash -> InitialTemperature -> FinalTemperature -> Duration -> Mash
doMash mash initialTemperature finalTemperature duration = DoMash initialTemperature finalTemperature duration mash

sparge :: Mash -> ResultingVolume -> Density -> Wort
sparge mash resultingVolume finalDensity = Sparge mash resultingVolume finalDensity

spargeWithEstimate :: Mash -> ResultingVolume -> Wort
spargeWithEstimate mash vol = sparge mash vol $ estimateDensityAfterSparge mash

addHops :: Wort -> Hops -> Weight -> Wort
addHops wort hops weight = AddHops hops weight wort

boil :: Wort -> Duration -> Wort
boil wort duration = Boil duration wort

boilWithEvaporation :: Wort -> Duration -> Wort
boilWithEvaporation wort duration = 
    let newVolume = getVolume wort - boilOff duration 
    in Concentrate newVolume $ Boil duration wort

chill :: Wort -> FinalTemperature -> Wort
chill wort finalTemperature = Chill finalTemperature wort

ferment :: Wort -> Temperature -> Duration -> Beer
ferment wort temperature duration = Ferment wort temperature duration

--    | AddHops Hops Weight Wort
--    | Boil Duration Wort
--    | Chill Duration FinalTemperature Wort
--    | Concentrate ResultingVolume Wort
--    | Dilute ResultingVolume Wort deriving (Show)

--boil :: Wort -> Duration -> Wort
--boil hoppedWort boilDuration = 
    -- let addDuration :: Duration -> (HopAmount, Duration) -> (HopAmount, Duration)
        -- addDuration boilTime (h, d) = (h, d + boilTime)

        -- newVolume :: Volume
        -- newVolume = (volume hoppedWort) - (boilOff boilDuration)
        
        -- newGravity = dilute (gravity hoppedWort) (volume hoppedWort) newVolume
    -- in 
        -- hoppedWort { hopsContent = map (addDuration boilDuration) $ hopsContent hoppedWort, volume = newVolume, gravity = newGravity }


-- dilute :: Density -> Volume -> Volume -> Density
-- dilute (Density oldDensity) (Milliliters oldVolume) (Milliliters newVolume) = Density $ (newVolume / oldVolume) * oldDensity

-- diluteWort :: Wort -> Volume -> Wort
-- diluteWort wort volume = wort --TODO: fix

-- utilization :: Density -> Duration -> Double
-- utilization (Density dens) (Minutes dur) = (1.65 * 0.000125**(dens - 1.0)) * ((1 - exp(-0.04 * dur)) / 4.15)

-- getBitterness :: Wort -> Bitterness
-- getBitterness wort =
    -- let
        -- partialBitterness :: (HopAmount, Duration) -> Bitterness
        -- partialBitterness (HopAmount hops (Grams weight), duration) = case (alphaContent hops, volume wort) of 
                                                                            -- (Percentage ac, Milliliters vol) -> IBU (ac * weight * (utilization (gravity wort) duration) / vol)
    -- in
        -- foldr (\(IBU x) (IBU y) -> IBU (x + y)) (IBU 0) $ map partialBitterness (hopsContent wort)

-- getABV :: Density -> Density -> ABV
-- getABV (Density og) (Density fg) = ABV (Percentage ((76.08 * (og-fg) / (1.775 - og) ) * (fg / 0.794)))

-- ferment :: Wort -> Density -> Beer
-- ferment hoppedwort finalgravity = Beer hoppedwort $ getABV (gravity hoppedwort) finalgravity -- TODO: calculate IBU from (hops,dur) and ABV from gravity
