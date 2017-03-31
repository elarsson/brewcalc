module BeerXML.ParseWater (
getWaters
)
where
import Data.Maybe
import Text.XML.Light
import Ingredients.Common
import Ingredients.Water
import BeerXML.Common

getWaters :: Element -> [[Water]]
getWaters = getIngredients "WATERS" "WATER" getWater


getWater :: Element -> Maybe Water
getWater e = 
        let
            notes = getString $ findElement QName { qName = "NOTES", qURI = Nothing, qPrefix = Nothing } e
            pH = getValue $ findElement QName { qName = "PH", qURI = Nothing, qPrefix = Nothing } e
        in
        do 
            name <- getString $ findElement QName { qName = "NAME", qURI = Nothing, qPrefix = Nothing } e
            version <- getValue $ findElement QName { qName = "VERSION", qURI = Nothing, qPrefix = Nothing } e
            amount <- getValue $ findElement QName { qName = "AMOUNT", qURI = Nothing, qPrefix = Nothing } e
            calcium <- getValue $ findElement QName { qName = "CALCIUM", qURI = Nothing, qPrefix = Nothing } e
            bicarbonate <- getValue $ findElement QName { qName = "BICARBONATE", qURI = Nothing, qPrefix = Nothing } e
            sulfate <- getValue $ findElement QName { qName = "SULFATE", qURI = Nothing, qPrefix = Nothing } e
            chloride <- getValue $ findElement QName { qName = "CHLORIDE", qURI = Nothing, qPrefix = Nothing } e
            sodium <- getValue $ findElement QName { qName = "SODIUM", qURI = Nothing, qPrefix = Nothing } e
            magnesium <- getValue $ findElement QName { qName = "MAGNESIUM", qURI = Nothing, qPrefix = Nothing } e
            return Water 
                {
                    Ingredients.Water.name = name,
                    Ingredients.Water.version = version,
                    Ingredients.Water.amount = amount,
                    Ingredients.Water.notes = notes,
                    Ingredients.Water.calcium = calcium,
                    Ingredients.Water.bicarbonate = bicarbonate,
                    Ingredients.Water.sulfate = sulfate,
                    Ingredients.Water.chloride = chloride,
                    Ingredients.Water.sodium = sodium,
                    Ingredients.Water.magnesium = magnesium,
                    Ingredients.Water.pH = pH
                }

