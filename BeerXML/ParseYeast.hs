module BeerXML.ParseYeast (
getYeasts
)
where
import Data.Maybe
import Text.XML.Light
import Ingredients.Common
import Ingredients.Yeast
import BeerXML.Common


getYeasts :: Element -> [[Yeast]]
getYeasts = getIngredients "YEASTS" "YEAST" getYeast

getYeastAmount :: CBool -> Maybe Element -> Maybe YeastAmount
getYeastAmount Ingredients.Common.True me =
        case (getValue me) of
            Nothing -> Nothing
            Just w -> return (YeastWeight w)
getYeastAmount Ingredients.Common.False me =
        case (getValue me) of
            Nothing -> Nothing
            Just w -> return (YeastVolume w)


getYeast :: Element -> Maybe Yeast
getYeast e = 
        do 
            name <- getString $ findElement QName { qName = "NAME", qURI = Nothing, qPrefix = Nothing } e
            version <- getValue $ findElement QName { qName = "VERSION", qURI = Nothing, qPrefix = Nothing } e
            yeastType <- getValue $ findElement QName { qName = "TYPE", qURI = Nothing, qPrefix = Nothing } e
            form <- getValue $ findElement QName { qName = "FORM", qURI = Nothing, qPrefix = Nothing } e
            amountIsWeight <- getValue $ findElement QName { qName = "AMOUNT_IS_WEIGHT", qURI = Nothing, qPrefix = Nothing } e
            amount <- getYeastAmount amountIsWeight $ findElement QName { qName = "AMOUNT", qURI = Nothing, qPrefix = Nothing } e
            return Yeast
                {
                    Ingredients.Yeast.name = name,
                    Ingredients.Yeast.version = version,
                    Ingredients.Yeast.yeastType = yeastType,
                    Ingredients.Yeast.amount = amount,
                    Ingredients.Yeast.form = form
                }
