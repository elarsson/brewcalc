module BeerXML.ParseRecipe (
getRecipes
)
where
import Data.Maybe
import Text.XML.Light
import Data.List.NonEmpty
import Ingredients.Common
import Ingredients.Mash
import Ingredients.Water
import Ingredients.Fermentable
import BeerXML.Common
import BeerXML.ParseHop
import BeerXML.ParseWater
import BeerXML.ParseYeast
import BeerXML.ParseFermentable

getRecipes :: Element -> [Mash]
getRecipes = getIngredients "RECIPES" "RECIPE" getRecipe

getRecipe :: Element -> Maybe Mash
getRecipe recipe =
    let
        hops = getHops recipe
        fermentables = getFermentables recipe
        waters = nonEmpty $ getWaters recipe
    in
    case waters of
        Nothing -> 
            case nonEmpty [mkTestWater "HEJ"] of 
                Nothing -> Nothing--Just (mkMash (nonEmpty [mkTestWater "HEJ"]))
                Just d -> Just $ mkMash d
        Just nl -> Just (mkMash nl)
    -- do
        -- waters <- nonEmpty $ getWaters recipe
        -- return $ mkMash waters
