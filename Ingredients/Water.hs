module Ingredients.Water (
Water(..),
mkTestWater
)
where
import Ingredients.Common
mkTestWater :: String -> Water
mkTestWater name = Water {
                    name = name,
                    version = 1,
                    amount = Milliliters 1000,
                    calcium = PPM 100,
                    bicarbonate = PPM 100,
                    sulfate = PPM 100,
                    chloride = PPM 100,
                    sodium = PPM 100,
                    magnesium = PPM 100,
                    pH = Just 7,
                    notes = Nothing
                }
data Water = Water
            {
                name :: String,
                version :: Int,
                amount :: Volume,
                calcium :: PPM,
                bicarbonate :: PPM,
                sulfate :: PPM,
                chloride :: PPM,
                sodium :: PPM,
                magnesium :: PPM,
                pH :: Maybe Double,
                notes :: Maybe String
            }
  deriving (Eq)
  
instance Show Water where
    show w = show $ name w
