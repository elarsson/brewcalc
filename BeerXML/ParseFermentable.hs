module BeerXML.ParseFermentable (
getFermentables
)
where
import Data.Maybe
import Text.XML.Light
import Ingredients.Common
import Ingredients.Fermentable
import BeerXML.Common

getFermentables :: Element -> [[Fermentable]]
getFermentables = getIngredients "FERMENTABLES" "FERMENTABLE" getFermentable


getFermentable :: Element -> Maybe Fermentable
getFermentable e = 
        let
            addAfterBoil = getValue $ findElement QName { qName = "ADD_AFTER_BOIL", qURI = Nothing, qPrefix = Nothing } e
            origin = getString $ findElement QName { qName = "ORIGIN", qURI = Nothing, qPrefix = Nothing } e
            supplier = getString $ findElement QName { qName = "SUPPLIER", qURI = Nothing, qPrefix = Nothing } e
            notes = getString $ findElement QName { qName = "NOTES", qURI = Nothing, qPrefix = Nothing } e
            coarseFineDiff = getValue $ findElement QName { qName = "COARSE_FINE_DIFF", qURI = Nothing, qPrefix = Nothing } e
            moisture = getValue $ findElement QName { qName = "MOISTURE", qURI = Nothing, qPrefix = Nothing } e
            diastaticPower = getValue $ findElement QName { qName = "DIASTATIC_POWER", qURI = Nothing, qPrefix = Nothing } e
            protein = getValue $ findElement QName { qName = "PROTEIN", qURI = Nothing, qPrefix = Nothing } e
            maxInBatch = getValue $ findElement QName { qName = "MAX_IN_BATCH", qURI = Nothing, qPrefix = Nothing } e
            recommendMash = getValue $ findElement QName { qName = "RECOMMEND_MASH", qURI = Nothing, qPrefix = Nothing } e
            ibuGalPerLb = getValue $ findElement QName { qName = "IBU_GAL_PER_LB", qURI = Nothing, qPrefix = Nothing } e
        in
        do 
            name <- getString $ findElement QName { qName = "NAME", qURI = Nothing, qPrefix = Nothing } e
            version <- getValue $ findElement QName { qName = "VERSION", qURI = Nothing, qPrefix = Nothing } e
            fermentableType <- getValue $ findElement QName { qName = "TYPE", qURI = Nothing, qPrefix = Nothing } e
            amount <- getValue $ findElement QName { qName = "AMOUNT", qURI = Nothing, qPrefix = Nothing } e
            yield <- getValue $ findElement QName { qName = "YIELD", qURI = Nothing, qPrefix = Nothing } e
            color <- getValue $ findElement QName { qName = "COLOR", qURI = Nothing, qPrefix = Nothing } e
            return Fermentable
                {
                    Ingredients.Fermentable.name = name,
                    Ingredients.Fermentable.version = version,
                    Ingredients.Fermentable.fermentableType = fermentableType,
                    Ingredients.Fermentable.amount = amount,
                    Ingredients.Fermentable.yield = yield,
                    Ingredients.Fermentable.color = color,
                    Ingredients.Fermentable.addAfterBoil = addAfterBoil,
                    Ingredients.Fermentable.origin = origin,
                    Ingredients.Fermentable.supplier = supplier,
                    Ingredients.Fermentable.notes = notes,
                    Ingredients.Fermentable.coarseFineDiff = coarseFineDiff,
                    Ingredients.Fermentable.moisture = moisture,
                    Ingredients.Fermentable.diastaticPower = diastaticPower,
                    Ingredients.Fermentable.protein = protein,
                    Ingredients.Fermentable.maxInBatch = maxInBatch,
                    Ingredients.Fermentable.recommendMash = recommendMash,
                    Ingredients.Fermentable.ibuGalPerLb = ibuGalPerLb
                }
