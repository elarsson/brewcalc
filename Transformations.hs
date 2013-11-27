module Transformations
( initiateMash
, addFermentable
, sparge
, boil
, ferment
) where
import Ingredients

initiateMash :: Volume -> Mash
initiateMash vol = Mash { fermentables = [], water = vol }

addFermentable :: Fermentable -> Weight -> Mash -> Mash
addFermentable ferm wt mash = Mash { fermentables = (ferm, wt) : fermentables mash, water = water mash }

sparge :: Mash -> Volume -> Density -> Wort
sparge mash vol density = Wort mash vol density

boil :: HoppedWort -> Duration -> HoppedWort
boil hoppedwort (Minutes mins) = 
    hoppedwort { hops = map (\(n, Minutes oldmins) -> (n, Minutes $ oldmins + mins)) $ hops hoppedwort} -- TODO, calculate boiloff and new density

ferment :: HoppedWort -> Density -> Beer
ferment hoppedwort finalgravity = Beer hoppedwort (IBU 0) 5 -- TODO: calculate IBU from (hops,dur) and ABV from gravity
