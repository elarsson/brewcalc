module Ingredients
( Weight
, Volume
, Fermentable(..)
, Mash(..)
, Wort(..)
, Beer(..)
, HoppedWort(..)
, Density
, Duration(..)
, Bitterness(..)
, Hops(..)
) where

-- Units
newtype Weight = Grams Integer
newtype Volume = Milliliters Integer
newtype Bitterness = IBU Integer
newtype Duration = Minutes Integer


type Percentage = Integer
type Efficiency = Percentage

type Density = Float

type ABV = Percentage

-- Components
data Fermentable =
     Grain { name :: String, fermentableContent :: Percentage } -- TODO: use content to calculate efficiency

data Hops = Hops { hopname :: String, alphacontent :: Percentage }

data Mash = Mash {  fermentables :: [(Fermentable, Weight)], water :: Volume }

data Wort = Wort Mash Volume Density

data HoppedWort = HoppedWort {  wort :: Wort, hops :: [(Hops, Duration)] }

data Beer = Beer HoppedWort Bitterness ABV
