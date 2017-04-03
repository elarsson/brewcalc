module BeerXML.Common (
 getString
,getValue
,getIngredientsFromXml
,getIngredients
)
where
import Text.Read
import Text.XML.Light
import Data.Maybe
import Control.Monad
import Ingredients.Mash
import Ingredients.Water
import Ingredients.Hop
import Ingredients.Common
import Debug.Trace



getIngredient :: String -> (Element -> Maybe a) -> Element -> [a]
getIngredient innerTag parseFn e =
            let 
                elements :: [Element]
                elements = (findElements QName { qName = innerTag, qURI = Nothing, qPrefix = Nothing } e)
            in
            case mapM parseFn elements of
                Nothing -> (trace $ "tomlista" ++ innerTag)[]
                Just l -> (trace "hello") l

getIngredients :: String -> String -> (Element -> Maybe a) -> Element -> [[a]]
getIngredients outerTag innerTag parseFn e =
            let elements = (findElements QName { qName = outerTag, qURI = Nothing, qPrefix = Nothing } e)
            in sequence $ Prelude.map (getIngredient innerTag parseFn) elements

getIngredientsFromXml :: (Element -> [a]) -> String -> [a]
getIngredientsFromXml parseFn xml =
    case (parseXMLDoc xml) of
        Nothing -> []
        Just e -> parseFn e

getString :: Maybe Element -> Maybe String
getString = liftM strContent

getValue :: (Read a) => Maybe Element -> Maybe a
getValue e = do
            strValue <- liftM strContent e
            readMaybe strValue






-- Read instances
instance Read CBool where
    readsPrec _ "TRUE" = return (Ingredients.Common.True, "")
    readsPrec _ "FALSE" = return (Ingredients.Common.False, "")
    readsPrec _ _ = []
instance Read Volume where
    readsPrec _ xs = case readMaybe xs of
                            Nothing -> []
                            Just d -> return (Milliliters d, "")
instance Read Duration where
    readsPrec _ xs = case readMaybe xs of
                            Nothing -> []
                            Just d -> return (Minutes d, "")
instance Read Weight where
    readsPrec _ xs = case readMaybe xs of
                            Nothing -> []
                            Just d -> return (Kilograms d, "")
instance Read Percentage where
    readsPrec _ xs = case readMaybe xs of
                            Nothing -> []
                            Just d -> return (Percentage d, "")
instance Read PPM where
    readsPrec _ xs = case readMaybe xs of
                            Nothing -> []
                            Just d -> return (PPM d, "")
instance Read Temperature where
    readsPrec _ xs = case readMaybe xs of
                            Nothing -> []
                            Just d -> return (Celsius d, "")