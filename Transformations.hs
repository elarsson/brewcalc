module Transformations
( mkMash
, addFermentable
, sparge
, boil
, ferment
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

sparge :: Mash -> Volume -> Density -> Wort
sparge mash vol density = Wort { mash = mash, volume = vol, gravity = density, hopsContent = [] }

boil :: Wort -> Duration -> Wort
boil hoppedWort boilDuration = 
    let addDuration :: Duration -> (Hops, Duration) -> (Hops, Duration)
        addDuration (Minutes boilTime) (h, Minutes d) = (h, Minutes $ d + boilTime)

        newVolume :: Volume
        newVolume = case (volume hoppedWort, (boilOff boilDuration)) of
                        ((Milliliters m), Milliliters m2) -> Milliliters $ m - m2
        
        newGravity = dilute (gravity hoppedWort) (volume hoppedWort) newVolume
    in 
        hoppedWort { hopsContent = map (addDuration boilDuration) $ hopsContent hoppedWort, volume = newVolume, gravity = newGravity }

boilOff :: Duration -> Volume
boilOff (Minutes m) = Milliliters $ m * (4000.0 / 60.0) -- TODO: don't hardcode

dilute :: Density -> Volume -> Volume -> Density
dilute (Density oldDensity) (Milliliters oldVolume) (Milliliters newVolume) = Density $ (newVolume / oldVolume) * oldDensity

ferment :: Wort -> Density -> Beer
ferment hoppedwort finalgravity = Beer hoppedwort $ ABV $ Percentage 5.0 -- TODO: calculate IBU from (hops,dur) and ABV from gravity
