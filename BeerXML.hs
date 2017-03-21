module BeerXML(
getHops,
getFermentables
) where 

import Ingredients
import IngredientsHops
import IngredientsFermentable
import Text.XML.Light
import Text.Read
import Data.Maybe
import Control.Monad

getIngredient :: String -> String -> (Element -> Maybe a) -> String -> [a]
getIngredient outerTag innerTag parseFn xml = 
    case (parseXMLDoc xml) of
        Nothing -> []
        Just e -> 
            let 
                elements :: [Element]
                elements = (findElements QName { qName = outerTag, qURI = Nothing, qPrefix = Nothing } e)

                mgh mm = mapM parseFn (findChildren QName { qName = innerTag, qURI = Nothing, qPrefix = Nothing } mm)
            in
            concat $ concat $ sequence $ map mgh elements

getHops :: String -> [Hop]
getHops = getIngredient "HOPS" "HOP" getHop

getFermentables :: String -> [Fermentable]
getFermentables = getIngredient "FERMENTABLES" "FERMENTABLE" getFermentable


getString :: Maybe Element -> Maybe String
getString = liftM strContent


getValue :: (Read a) => Maybe Element -> Maybe a
getValue e = do
            strValue <- liftM strContent e
            readMaybe strValue
           
getHop :: Element -> Maybe Hop
getHop e = 
        let
            notes = getString $ findElement QName { qName = "NOTES", qURI = Nothing, qPrefix = Nothing } e
            usedAsType = getValue $ findElement QName { qName = "TYPE", qURI = Nothing, qPrefix = Nothing } e
            form = getValue $ findElement QName { qName = "FORM", qURI = Nothing, qPrefix = Nothing } e
            beta = getValue $ findElement QName { qName = "BETA", qURI = Nothing, qPrefix = Nothing } e
            hSI = getValue $ findElement QName { qName = "HSI", qURI = Nothing, qPrefix = Nothing } e
            origin = getString $ findElement QName { qName = "ORIGIN", qURI = Nothing, qPrefix = Nothing } e
            substitutes = getString $ findElement QName { qName = "SUBSTITUTES", qURI = Nothing, qPrefix = Nothing } e
            humulene = getValue $ findElement QName { qName = "HUMULENE", qURI = Nothing, qPrefix = Nothing } e
            caryophyllene = getValue $ findElement QName { qName = "CARYOPHYLLENE", qURI = Nothing, qPrefix = Nothing } e
            cohumulone = getValue $ findElement QName { qName = "COHUMULONE", qURI = Nothing, qPrefix = Nothing } e
            myrcene = getValue $ findElement QName { qName = "MYRCENE", qURI = Nothing, qPrefix = Nothing } e
        in
        do 
            name <- getString $ findElement QName { qName = "NAME", qURI = Nothing, qPrefix = Nothing } e
            version <- getValue $ findElement QName { qName = "VERSION", qURI = Nothing, qPrefix = Nothing } e
            alpha <- getValue $ findElement QName { qName = "ALPHA", qURI = Nothing, qPrefix = Nothing } e
            amount <- getValue $ findElement QName { qName = "AMOUNT", qURI = Nothing, qPrefix = Nothing } e
            use <- getValue $ findElement QName { qName = "USE", qURI = Nothing, qPrefix = Nothing } e
            time <- getValue $ findElement QName { qName = "TIME", qURI = Nothing, qPrefix = Nothing } e
            return Hop 
                {
                    IngredientsHops.name = name,
                    alpha = alpha,
                    IngredientsHops.version = version,
                    IngredientsHops.amount = amount,
                    use = use,
                    time = time,
                    IngredientsHops.notes = notes,
                    usedAsType = usedAsType,
                    form = form,
                    beta = beta,
                    hSI = hSI,
                    IngredientsHops.origin = origin,
                    substitutes = substitutes,
                    humulene = humulene,
                    caryophyllene = caryophyllene,
                    cohumulone = cohumulone,
                    myrcene = myrcene
                }


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
                    IngredientsFermentable.name = name,
                    IngredientsFermentable.version = version,
                    IngredientsFermentable.fermentableType = fermentableType,
                    IngredientsFermentable.amount = amount,
                    IngredientsFermentable.yield = yield,
                    IngredientsFermentable.color = color,
                    IngredientsFermentable.addAfterBoil = addAfterBoil,
                    IngredientsFermentable.origin = origin,
                    IngredientsFermentable.supplier = supplier,
                    IngredientsFermentable.notes = notes,
                    IngredientsFermentable.coarseFineDiff = coarseFineDiff,
                    IngredientsFermentable.moisture = moisture,
                    IngredientsFermentable.diastaticPower = diastaticPower,
                    IngredientsFermentable.protein = protein,
                    IngredientsFermentable.maxInBatch = maxInBatch,
                    IngredientsFermentable.recommendMash = recommendMash,
                    IngredientsFermentable.ibuGalPerLb = ibuGalPerLb
                }
