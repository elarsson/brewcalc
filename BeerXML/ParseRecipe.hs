module BeerXML.ParseRecipe (
getRecipes
,Recipe(..)
)
where
import Data.Maybe
import Text.XML.Light
import Data.List.NonEmpty
import Ingredients.Common
import Ingredients.Mash
import Ingredients.Hop
import Ingredients.Water
import Ingredients.Fermentable
import BeerXML.Common
import BeerXML.ParseHop
import BeerXML.ParseWater
import BeerXML.ParseYeast
import BeerXML.ParseFermentable
import BeerXML.ParseMash
import Debug.Trace

getRecipes :: Element -> [[Recipe]]
getRecipes = getIngredients "RECIPES" "RECIPE" getRecipe

data Recipe = Recipe {
        -- hops :: NonEmpty Hop--,
        -- fermentables :: NonEmpty Fermentable,
        -- waters :: [Water],
        mashProfile :: MashProfile
    } deriving (Show,Eq)

getRecipe :: Element -> Maybe Recipe
getRecipe recipe =
    let
        -- hopsM = concat $ getHops recipe
        -- fermentables = concat $ getFermentables recipe
        -- waters = concat $ getWaters recipe -- Optional record
        mashProfiles = listToMaybe (getMashProfiles recipe) -- Only use the first one
    in
    do
        allMashProfiles <- (trace $ show (getMashProfiles recipe)) mashProfiles
        mashProfile <- listToMaybe allMashProfiles
        -- nefermentables <- nonEmpty fermentables
        -- nehops <- nonEmpty hopsM
        return Recipe
            {
                mashProfile = mashProfile
                -- waters = waters,
                -- hops = nehops
                -- fermentables = nefermentables
            }
