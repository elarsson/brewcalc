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
sparge mash vol density = Wort mash vol density

boil :: HoppedWort -> Duration -> HoppedWort
boil hoppedwort boilDuration = 
    let addDuration :: Duration -> (Hops, Duration) -> (Hops, Duration)
        addDuration (Minutes boilTime) (h, Minutes d) = (h, Minutes $ d + boilTime)
    in 
        hoppedwort { hops = map (addDuration boilDuration) $ hops hoppedwort} -- TODO, calculate boiloff and new density

ferment :: HoppedWort -> Density -> Beer
ferment hoppedwort finalgravity = Beer hoppedwort (IBU 0) 5 -- TODO: calculate IBU from (hops,dur) and ABV from gravity
