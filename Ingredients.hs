module Ingredients
( Weight
, Volume
, Fermentable(..)
, Mash(..)
) where

newtype Weight = Grams Integer
newtype Volume = Milliliters Integer
type Percentage = Integer
type Efficiency = Percentage

data Fermentable = Fermentable	{  name :: String,
								   fermentableContent :: Percentage
								}


data Mash = Mash { 	fermentables :: [Fermentable -> Weight],
					water :: Volume
				 }

data Wort = Wort Mash Volume Efficiency
