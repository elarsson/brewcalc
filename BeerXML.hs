module BeerXML(
getHops,
getFermentables,
getYeasts,
getWaters,
getWatersFromXml,
getRecipes,
getRecipesFromXml,
getMashProfilesFromXml
) where 

import Ingredients
import IngredientsHops
import IngredientsFermentable
import IngredientsYeast
import IngredientsMash
import IngredientsWater
import Text.XML.Light
import Text.Read
import Data.Maybe
import Control.Monad
import Data.List.NonEmpty

getMashProfile :: Element -> Maybe MashProfile
getMashProfile profile =
    Just (TestMashProfile "HEJ") -- TODO call read mash step function

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
    
    

getIngredient :: String -> (Element -> Maybe a) -> Element -> [a]
getIngredient innerTag parseFn e =
            let 
                elements :: [Element]
                elements = (findElements QName { qName = innerTag, qURI = Nothing, qPrefix = Nothing } e)
            in
            case mapM parseFn elements of
                Nothing -> []
                Just l -> l

getIngredients :: String -> String -> (Element -> Maybe a) -> Element -> [a]
getIngredients outerTag innerTag parseFn e =
            let elements = (findElements QName { qName = outerTag, qURI = Nothing, qPrefix = Nothing } e)
            in concat $ sequence $ Prelude.map (getIngredient innerTag parseFn) elements

getIngredientsFromXml :: String -> String -> (Element -> Maybe a) -> String -> [a]
getIngredientsFromXml outerTag innerTag parseFn xml =
    case (parseXMLDoc xml) of
        Nothing -> []
        Just e -> getIngredients outerTag innerTag parseFn e

getMashProfilesFromXml :: String -> [MashProfile]
getMashProfilesFromXml = getIngredientsFromXml "MASHS" "MASH" getMashProfile

getRecipesFromXml :: String -> [Mash]
getRecipesFromXml = getIngredientsFromXml "RECIPES" "RECIPE" getRecipe

getRecipes :: Element -> [Mash]
getRecipes = getIngredients "RECIPES" "RECIPE" getRecipe

getHops :: Element -> [Hop]
getHops = getIngredients "HOPS" "HOP" getHop

getWaters :: Element -> [Water]
getWaters = getIngredients "WATERS" "WATER" getWater
getWatersFromXml :: String -> [Water]
getWatersFromXml = getIngredientsFromXml "WATERS" "WATER" getWater

getFermentables :: Element -> [Fermentable]
getFermentables = getIngredients "FERMENTABLES" "FERMENTABLE" getFermentable

getYeasts :: Element -> [Yeast]
getYeasts = getIngredients "YEASTS" "YEAST" getYeast

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
                    IngredientsHops.alpha = alpha,
                    IngredientsHops.version = version,
                    IngredientsHops.amount = amount,
                    IngredientsHops.use = use,
                    IngredientsHops.time = time,
                    IngredientsHops.notes = notes,
                    IngredientsHops.usedAsType = usedAsType,
                    IngredientsHops.form = form,
                    IngredientsHops.beta = beta,
                    IngredientsHops.hSI = hSI,
                    IngredientsHops.origin = origin,
                    IngredientsHops.substitutes = substitutes,
                    IngredientsHops.humulene = humulene,
                    IngredientsHops.caryophyllene = caryophyllene,
                    IngredientsHops.cohumulone = cohumulone,
                    IngredientsHops.myrcene = myrcene
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


    
    
    
    
getYeastAmount :: CBool -> Maybe Element -> Maybe YeastAmount
getYeastAmount Ingredients.True me =
        case (getValue me) of
            Nothing -> Nothing
            Just w -> return (YeastWeight w)
getYeastAmount Ingredients.False me =
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
                    IngredientsYeast.name = name,
                    IngredientsYeast.version = version,
                    IngredientsYeast.yeastType = yeastType,
                    IngredientsYeast.amount = amount,
                    IngredientsYeast.form = form
                }

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
                    IngredientsWater.name = name,
                    IngredientsWater.version = version,
                    IngredientsWater.amount = amount,
                    IngredientsWater.notes = notes,
                    IngredientsWater.calcium = calcium,
                    IngredientsWater.bicarbonate = bicarbonate,
                    IngredientsWater.sulfate = sulfate,
                    IngredientsWater.chloride = chloride,
                    IngredientsWater.sodium = sodium,
                    IngredientsWater.magnesium = magnesium,
                    IngredientsWater.pH = pH
                }
