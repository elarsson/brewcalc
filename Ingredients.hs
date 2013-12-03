module Ingredients
( Weight(..)
, Density(..)
, Volume(..)
, Fermentable(..)
, Mash(..)
, Wort(..)
, Beer(..)
, Duration(..)
, Bitterness(..)
, Hops(..)
, Percentage(..)
, ABV(..)
) where

-- Units
newtype Weight = Grams Float
newtype Volume = Milliliters Float
newtype Bitterness = IBU Float
newtype Duration = Minutes Float
newtype Density = Density Float
newtype Percentage = Percentage Float
newtype Efficiency = Efficiency Percentage
newtype ABV = ABV Percentage

-- Components
data Fermentable =
     Grain { grainName :: String, fermentableContent :: Percentage } -- TODO: use content to calculate efficiency

data Hops = Hops { hopName :: String, alphaContent :: Percentage }

data Mash = Mash { fermentables :: [(Fermentable, Weight)], water :: Volume }

data Wort = Wort { mash :: Mash, volume :: Volume, gravity :: Density, hopsContent :: [(Hops, Duration)] } -- TODO: need amount of hops

data Beer = Beer Wort ABV
