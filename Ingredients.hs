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
, Efficiency(..)
, ABV(..)
, HopAmount(..)
, ResultingVolume(..)
, InitialTemperature(..)
, FinalTemperature(..)
, Temperature(..)
) where

-- Units
newtype Volume = Milliliters Double
instance Num Volume where
    (+) (Milliliters w1) (Milliliters w2) = Milliliters (w1 + w2)
    (*) (Milliliters w1) (Milliliters w2) = Milliliters (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (Milliliters w) = Milliliters (0 - w)
    abs (Milliliters w) = Milliliters (abs w)
    signum (Milliliters w) = Milliliters (signum w)
    fromInteger i = Milliliters (fromInteger i)
instance Show Volume where
    show (Milliliters a) = show a ++ "ml"

newtype Bitterness = IBU Double
instance Num Bitterness where
    (+) (IBU w1) (IBU w2) = IBU (w1 + w2)
    (*) (IBU w1) (IBU w2) = IBU (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (IBU w) = IBU (0 - w)
    abs (IBU w) = IBU (abs w)
    signum (IBU w) = IBU (signum w)
    fromInteger i = IBU (fromInteger i)

newtype Duration = Minutes Double deriving (Show)
instance Num Duration where
    (+) (Minutes w1) (Minutes w2) = Minutes (w1 + w2)
    (*) (Minutes w1) (Minutes w2) = Minutes (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (Minutes w) = Minutes (0 - w)
    abs (Minutes w) = Minutes (abs w)
    signum (Minutes w) = Minutes (signum w)
    fromInteger i = Minutes (fromInteger i)

newtype Density = Density Double deriving (Show)
instance Num Density where
    (+) (Density w1) (Density w2) = Density (w1 + w2)
    (*) (Density w1) (Density w2) = Density (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (Density w) = Density (0 - w)
    abs (Density w) = Density (abs w)
    signum (Density w) = Density (signum w)
    fromInteger i = Density (fromInteger i)

newtype Weight = Grams Double
instance Show Weight where
    show (Grams a) = show a ++ "g"
instance Num Weight where
    (+) (Grams w1) (Grams w2) = Grams (w1 + w2)
    (*) (Grams w1) (Grams w2) = Grams (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (Grams w) = Grams (0 - w)
    abs (Grams w) = Grams (abs w)
    signum (Grams w) = Grams (signum w)
    fromInteger i = Grams (fromInteger i)

newtype Percentage = Percentage Double
instance Show Percentage where
    show (Percentage a) = show a ++ "%"

newtype Efficiency = Efficiency Percentage deriving (Show)
newtype Temperature = Celsius Double deriving (Show)
newtype ABV = ABV Percentage

data HopAmount = HopAmount Hops Weight deriving (Show)
type ResultingVolume = Volume
type InitialTemperature = Temperature
type FinalTemperature = Temperature

-- Components
data Fermentable =
     Grain { grainName :: String, extractPotential :: Efficiency } deriving Show

data Hops = Hops { hopName :: String, alphaContent :: Percentage } deriving Show

data Wort = Sparge Mash ResultingVolume Density
    | AddHops Hops Weight Wort
    | Boil Duration Wort
    | Chill Duration FinalTemperature Wort
    | Concentrate ResultingVolume Wort
    | Dilute ResultingVolume Wort deriving (Show)

data Mash = AddWater Volume
    | AddGrain Fermentable Weight Mash
    | DoMash InitialTemperature FinalTemperature Duration Mash deriving (Show)

data Beer = Ferment Wort Temperature Duration deriving (Show)
