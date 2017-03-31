module Ingredients
( Weight(..)
, Density(..)
, Volume(..)
--, Fermentable(..)
--, Mash(..)
--, Wort(..)
--, Beer(..)
, Duration(..)
, Bitterness(..)
--, Hops(..)
, Percentage(..)
, Efficiency(..)
, ABV(..)
--, HopAmount(..)
, ResultingVolume(..)
, InitialTemperature(..)
, FinalTemperature(..)
, Temperature(..)
, CBool(..)
, PPM(..)
) where
import Text.Read

-- Units
newtype Volume = Milliliters Double deriving (Eq)
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
instance Read Volume where
    readsPrec _ xs = case readMaybe xs of
                            Nothing -> []
                            Just d -> return (Milliliters d, "")

newtype Bitterness = IBU Double
instance Num Bitterness where
    (+) (IBU w1) (IBU w2) = IBU (w1 + w2)
    (*) (IBU w1) (IBU w2) = IBU (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (IBU w) = IBU (0 - w)
    abs (IBU w) = IBU (abs w)
    signum (IBU w) = IBU (signum w)
    fromInteger i = IBU (fromInteger i)

newtype Duration = Minutes Double deriving (Eq, Show)
instance Num Duration where
    (+) (Minutes w1) (Minutes w2) = Minutes (w1 + w2)
    (*) (Minutes w1) (Minutes w2) = Minutes (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (Minutes w) = Minutes (0 - w)
    abs (Minutes w) = Minutes (abs w)
    signum (Minutes w) = Minutes (signum w)
    fromInteger i = Minutes (fromInteger i)
instance Read Duration where
    readsPrec _ xs = case readMaybe xs of
                            Nothing -> []
                            Just d -> return (Minutes d, "")

newtype Density = Density Double deriving (Show)
instance Num Density where
    (+) (Density w1) (Density w2) = Density (w1 + w2)
    (*) (Density w1) (Density w2) = Density (w1 * w2)
    (-) w1 w2 = w1 + negate w2
    negate (Density w) = Density (0 - w)
    abs (Density w) = Density (abs w)
    signum (Density w) = Density (signum w)
    fromInteger i = Density (fromInteger i)

newtype Weight = Kilograms Double deriving (Eq)
instance Show Weight where
    show (Kilograms a) = show a ++ "kg"
instance Read Weight where
    readsPrec _ xs = case readMaybe xs of
                            Nothing -> []
                            Just d -> return (Kilograms d, "")

newtype Percentage = Percentage Double deriving (Eq)
instance Show Percentage where
    show (Percentage a) = show a ++ "%"
instance Read Percentage where
    readsPrec _ xs = case readMaybe xs of
                            Nothing -> []
                            Just d -> return (Percentage d, "")

newtype PPM = PPM Double deriving (Eq)
instance Show PPM where
    show (PPM a) = show a ++ "ppm"
instance Read PPM where
    readsPrec _ xs = case readMaybe xs of
                            Nothing -> []
                            Just d -> return (PPM d, "")

newtype Efficiency = Efficiency Percentage deriving (Show)
newtype Temperature = Celsius Double deriving (Show, Eq)
instance Read Temperature where
    readsPrec _ xs = case readMaybe xs of
                            Nothing -> []
                            Just d -> return (Celsius d, "")
newtype ABV = ABV Percentage

--data HopAmount = HopAmount Hops Weight deriving (Show)
type ResultingVolume = Volume
type InitialTemperature = Temperature
type FinalTemperature = Temperature

data CBool = True | False deriving (Show, Eq)
instance Read CBool where
    readsPrec _ "TRUE" = return (Ingredients.True, "")
    readsPrec _ "FALSE" = return (Ingredients.False, "")
    readsPrec _ _ = []

--data Hops = Hops { hopName :: String, alphaContent :: Percentage } deriving Show

-- data Wort = Sparge Mash ResultingVolume Density
    -- | AddHops Hop Weight Wort
    -- | Boil Duration Wort
    -- | Chill FinalTemperature Wort
    -- | Concentrate ResultingVolume Wort
    -- | Dilute ResultingVolume Wort deriving (Show)

-- data Mash = AddWater Volume
    -- | AddGrain Fermentable Weight Mash
    -- | DoMash InitialTemperature FinalTemperature Duration Mash deriving (Show)

-- data Beer = Ferment Wort Temperature Duration deriving (Show)
