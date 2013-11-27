module Ingredients
( Weight
, Volume
, Fermentable(..)
, Mash(..)
, Wort(..)
, Beer(..)
) where

newtype Weight = Grams Integer
newtype Volume = Milliliters Integer
type Percentage = Integer
type Efficiency = Percentage
type Density = Float
newtype Duration = Minutes Integer
newtype Bitterness = IBU Integer
type ABV = Percentage

data Fermentable = Fermentable	{  name :: String,
								   fermentableContent :: Percentage -- TODO: use to calculate efficiency
								}

data Hops = Hops { 	hopname :: String,
					alphacontent :: Percentage
				 }

data Mash = Mash { 	fermentables :: [(Fermentable, Weight)],
					water :: Volume
				 }

data Wort = Wort Mash Volume Density

data HoppedWort = HoppedWort { 	wort :: Wort,
								hops :: [(Hops, Duration)]
							 }

data Beer = Beer HoppedWort Bitterness ABV

