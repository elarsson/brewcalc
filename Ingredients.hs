module Ingredients
( Grams
, Milliliters
, Fermentable
, Mash
) where

type Grams = Integer
type Milliliters = Integer

data Fermentable = Fermentable	{  name :: String,
								   fermentableContent :: Integer
								}


data Mash = Mash { 	fermentables :: [Fermentable -> Grams],
					water :: Milliliters
				 }