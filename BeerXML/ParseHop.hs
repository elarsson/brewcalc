module BeerXML.ParseHop (
getHops
)
where
import Data.Maybe
import Text.XML.Light
import Ingredients.Common
import Ingredients.Hop
import BeerXML.Common

getHops :: Element -> [Hop]
getHops = getIngredients "HOPS" "HOP" getHop

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
                    Ingredients.Hop.name = name,
                    Ingredients.Hop.alpha = alpha,
                    Ingredients.Hop.version = version,
                    Ingredients.Hop.amount = amount,
                    Ingredients.Hop.use = use,
                    Ingredients.Hop.time = time,
                    Ingredients.Hop.notes = notes,
                    Ingredients.Hop.usedAsType = usedAsType,
                    Ingredients.Hop.form = form,
                    Ingredients.Hop.beta = beta,
                    Ingredients.Hop.hSI = hSI,
                    Ingredients.Hop.origin = origin,
                    Ingredients.Hop.substitutes = substitutes,
                    Ingredients.Hop.humulene = humulene,
                    Ingredients.Hop.caryophyllene = caryophyllene,
                    Ingredients.Hop.cohumulone = cohumulone,
                    Ingredients.Hop.myrcene = myrcene
                }
