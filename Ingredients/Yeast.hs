module Ingredients.Yeast (
Yeast(..),
YeastAmount(..)
)
where
import Ingredients.Common


data YeastType = Ale | Lager | Wheat | Wine | Champagne deriving (Show, Eq, Read)
data YeastForm = Liquid | Dry | Slant | Culture deriving (Show, Eq, Read)
data YeastAmount = YeastWeight Weight | YeastVolume Volume deriving (Show, Eq)

data Yeast = Yeast
            {
                name :: String,
                version :: Int,
                yeastType :: YeastType,
                form :: YeastForm,
                amount :: YeastAmount
            }
  deriving (Show, Eq)
