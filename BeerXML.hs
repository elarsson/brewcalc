module BeerXML(
getHops
) where 

--import Ingredients
import Text.XML.Light
import Text.Read
import Data.Maybe
import Control.Monad

getHops :: String -> [Hop]
getHops s = 
    case (parseXMLDoc s) of
        Nothing -> []
        Just e -> 
            let 
                hopsElements :: [Element]
                hopsElements = (findElements QName { qName = "HOPS", qURI = Nothing, qPrefix = Nothing } e)

                mgh :: Element -> Maybe [Hop]
                mgh mm = mapM getHop (findChildren QName { qName = "HOP", qURI = Nothing, qPrefix = Nothing } mm)
            in
            concat $ concat $ sequence $ map mgh hopsElements

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
            return Hop { name = name, alpha = alpha, version = version, amount = amount, use = use, time = time, notes = notes, usedAsType = usedAsType, form = form, beta = beta, hSI = hSI, origin = origin, substitutes = substitutes, humulene = humulene, caryophyllene = caryophyllene, cohumulone = cohumulone, myrcene = myrcene}

newtype Weight = Kilograms Double deriving (Eq)
instance Show Weight where
    show (Kilograms a) = show a ++ "kg"
instance Read Weight where
    readsPrec _ xs = let 
                        a :: Double
                        a = read xs
                    in
                        return (Kilograms a, "")

data Use = Boil | DryHop | Mash | FirstWort | Aroma deriving (Show, Eq)
instance Read Use where
    readsPrec _ "Boil" = return (Boil, "")
    readsPrec _ "DryHop" = return (DryHop, "")
    readsPrec _ "Mash" = return (Mash, "")
    readsPrec _ "FirstWort" = return (FirstWort, "")
    readsPrec _ "Aroma" = return (Aroma, "")
    readsPrec _ _ = []

data UsedAs = UsedAsBittering | UsedAsAroma | UsedAsBoth deriving (Show, Eq)
instance Read UsedAs where
    readsPrec _ "Bittering" = return (UsedAsBittering, "")
    readsPrec _ "Aroma" = return (UsedAsAroma, "")
    readsPrec _ "Both" = return (UsedAsBoth, "")
    readsPrec _ _ = []

data Form = Pellet | Plug | Leaf deriving (Show, Eq)
instance Read Form where
    readsPrec _ "Pellet" = return (Pellet, "")
    readsPrec _ "Plug" = return (Plug, "")
    readsPrec _ "Leaf" = return (Leaf, "")
    readsPrec _ _ = []

type Percentage = Double

data Hop = Hop { name :: String,
                 version :: Int,
                 alpha :: Percentage,
                 amount :: Weight,
                 use :: Use,
                 time :: Double,
                 notes :: Maybe String,
                 usedAsType :: Maybe UsedAs,
                 form :: Maybe Form,
                 beta :: Maybe Percentage,
                 hSI :: Maybe Percentage,
                 origin :: Maybe String,
                 substitutes :: Maybe String,
                 humulene :: Maybe Percentage,
                 caryophyllene :: Maybe Percentage,
                 cohumulone :: Maybe Percentage,
                 myrcene :: Maybe Percentage

                 -- displayAmount :: Maybe String,
                 -- displayTime :: String
                 -- inventory :: String,
                 }
  deriving (Show, Eq)
  
  
   -- <NAME>Cascade</NAME>
 -- <VERSION>1</VERSION>
 -- <ORIGIN>US</ORIGIN>
 -- <ALPHA>5.50</ALPHA>
 -- <AMOUNT>0.0000000</AMOUNT>
 -- <USE>Boil</USE>
 -- <TIME>0.000</TIME>
 -- <NOTES>Use For: American ales and lagers
-- Aroma: Strong spicy, floral, grapefriut character
-- Substitutes: Centennial
-- Examples: Sierra Nevade Pale Ale, Anchor Liberty Ale
-- A hops with Northern Brewers Heritage</NOTES>
 -- <TYPE>Both</TYPE>
 -- <FORM>Pellet</FORM>
 -- <BETA>6.00</BETA>
 -- <HSI>50.0</HSI>
 -- <DISPLAY_AMOUNT>0.00 oz</DISPLAY_AMOUNT>
 -- <INVENTORY>0.00 oz</INVENTORY>
 -- <DISPLAY_TIME>-</DISPLAY_TIME>
