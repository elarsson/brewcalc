module BeerXML.Parser (
 getMashProfilesFromXml
,getRecipesFromXml
,getWatersFromXml
,getHopsFromXml
)
where
import BeerXML.Common
import BeerXML.ParseMash
import BeerXML.ParseRecipe
import BeerXML.ParseWater
import BeerXML.ParseHop

import Ingredients.Mash
import Ingredients.Water
import Ingredients.Hop


getMashProfilesFromXml :: String -> [[MashProfile]]
getMashProfilesFromXml = getIngredientsFromXml getMashProfiles

getRecipesFromXml :: String -> [[Recipe]]
getRecipesFromXml = getIngredientsFromXml getRecipes

getWatersFromXml :: String -> [[Water]]
getWatersFromXml = getIngredientsFromXml getWaters

getHopsFromXml :: String -> [[Hop]]
getHopsFromXml = getIngredientsFromXml getHops

