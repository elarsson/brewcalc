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
, HopAmount(..)
) where

-- Units
newtype Volume = Milliliters Float
instance Num Volume where
    (+) (Milliliters w1) (Milliliters w2) = Milliliters (w1 + w2)
    (*) (Milliliters w1) (Milliliters w2) = Milliliters (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (Milliliters w) = Milliliters (0 - w)
    abs (Milliliters w) = Milliliters (abs w)
    signum (Milliliters w) = Milliliters (signum w)
    fromInteger i = Milliliters (fromInteger i)

newtype Bitterness = IBU Float
instance Num Bitterness where
    (+) (IBU w1) (IBU w2) = IBU (w1 + w2)
    (*) (IBU w1) (IBU w2) = IBU (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (IBU w) = IBU (0 - w)
    abs (IBU w) = IBU (abs w)
    signum (IBU w) = IBU (signum w)
    fromInteger i = IBU (fromInteger i)

newtype Duration = Minutes Float
instance Num Duration where
    (+) (Minutes w1) (Minutes w2) = Minutes (w1 + w2)
    (*) (Minutes w1) (Minutes w2) = Minutes (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (Minutes w) = Minutes (0 - w)
    abs (Minutes w) = Minutes (abs w)
    signum (Minutes w) = Minutes (signum w)
    fromInteger i = Minutes (fromInteger i)

newtype Density = Density Float
instance Num Density where
    (+) (Density w1) (Density w2) = Density (w1 + w2)
    (*) (Density w1) (Density w2) = Density (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (Density w) = Density (0 - w)
    abs (Density w) = Density (abs w)
    signum (Density w) = Density (signum w)
    fromInteger i = Density (fromInteger i)

newtype Weight = Grams Float
instance Num Weight where
    (+) (Grams w1) (Grams w2) = Grams (w1 + w2)
    (*) (Grams w1) (Grams w2) = Grams (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (Grams w) = Grams (0 - w)
    abs (Grams w) = Grams (abs w)
    signum (Grams w) = Grams (signum w)
    fromInteger i = Grams (fromInteger i)

newtype Percentage = Percentage Float
newtype Efficiency = Efficiency Percentage
newtype ABV = ABV Percentage
data HopAmount = HopAmount Hops Weight


-- Components
data Fermentable =
     Grain { grainName :: String, fermentableContent :: Percentage } -- TODO: use content to calculate efficiency

data Hops = Hops { hopName :: String, alphaContent :: Percentage }

data Mash = Mash { fermentables :: [(Fermentable, Weight)], water :: Volume }

data Wort = Wort { mash :: Mash, volume :: Volume, gravity :: Density, hopsContent :: [(HopAmount, Duration)] } -- TODO: need amount of hops

data Beer = Beer Wort ABV
