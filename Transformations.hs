module Transformations
( mkMash
, addFermentable
, sparge
, boil
, ferment
, utilization
) where
import Ingredients

mkMash :: Volume -> Mash
mkMash vol = Mash { fermentables = [], water = vol }

addFermentable :: Fermentable -> Weight -> Mash -> Mash
addFermentable ferm wt mash =
    Mash { 
            fermentables = (ferm, wt) : fermentables mash,
            water = water mash
         }

--mash :: Mash -> Temperature -> Temperature -> Duration
--mash m startTemperature endTemperature duration = m

sparge :: Mash -> Volume -> Density -> Wort
sparge mash resultingVolume resultingDensity = Wort { mash = mash, volume = resultingVolume, gravity = resultingDensity, hopsContent = [] }

boil :: Wort -> Duration -> Wort
boil hoppedWort boilDuration = 
    let addDuration :: Duration -> (HopAmount, Duration) -> (HopAmount, Duration)
        addDuration boilTime (h, d) = (h, d + boilTime)

        newVolume :: Volume
        newVolume = (volume hoppedWort) - (boilOff boilDuration)
        
        newGravity = dilute (gravity hoppedWort) (volume hoppedWort) newVolume
    in 
        hoppedWort { hopsContent = map (addDuration boilDuration) $ hopsContent hoppedWort, volume = newVolume, gravity = newGravity }

boilOff :: Duration -> Volume
boilOff (Minutes m) = Milliliters $ m * (4000.0 / 60.0) -- TODO: 4 liters/hour... don't hardcode

dilute :: Density -> Volume -> Volume -> Density
dilute (Density oldDensity) (Milliliters oldVolume) (Milliliters newVolume) = Density $ (newVolume / oldVolume) * oldDensity

diluteWort :: Wort -> Volume -> Wort
diluteWort wort volume = wort --TODO: fix

utilization :: Density -> Duration -> Double
utilization (Density dens) (Minutes dur) = (1.65 * 0.000125**(dens - 1.0)) * ((1 - exp(-0.04 * dur)) / 4.15)

getBitterness :: Wort -> Bitterness
getBitterness wort =
    let
        partialBitterness :: (HopAmount, Duration) -> Bitterness
        partialBitterness (HopAmount hops (Grams weight), duration) = case (alphaContent hops, volume wort) of 
                                                                            (Percentage ac, Milliliters vol) -> IBU (ac * weight * (utilization (gravity wort) duration) / vol)
    in
        foldr (\(IBU x) (IBU y) -> IBU (x + y)) (IBU 0) $ map partialBitterness (hopsContent wort)

getABV :: Density -> Density -> ABV
getABV (Density og) (Density fg) = ABV (Percentage ((76.08 * (og-fg) / (1.775 - og) ) * (fg / 0.794)))

ferment :: Wort -> Density -> Beer
ferment hoppedwort finalgravity = Beer hoppedwort $ getABV (gravity hoppedwort) finalgravity -- TODO: calculate IBU from (hops,dur) and ABV from gravity
