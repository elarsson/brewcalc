module Ingredients.Hop (
Hop(..)
) where 
import Ingredients.Common

data Use = UseBoil | DryHop | Mash | FirstWort | Aroma deriving (Show, Eq)
instance Read Use where
    readsPrec _ "Boil" = return (UseBoil, "")
    readsPrec _ "Dry Hop" = return (DryHop, "")
    readsPrec _ "Mash" = return (Mash, "")
    readsPrec _ "First Wort" = return (FirstWort, "")
    readsPrec _ "Aroma" = return (Aroma, "")
    readsPrec _ _ = []

data UsedAs = UsedAsBittering | UsedAsAroma | UsedAsBoth deriving (Show, Eq)
instance Read UsedAs where
    readsPrec _ "Bittering" = return (UsedAsBittering, "")
    readsPrec _ "Aroma" = return (UsedAsAroma, "")
    readsPrec _ "Both" = return (UsedAsBoth, "")
    readsPrec _ _ = []

data Form = Pellet | Plug | Leaf deriving (Show, Eq)
instance Read Form where
    readsPrec _ "Pellet" = return (Pellet, "")
    readsPrec _ "Plug" = return (Plug, "")
    readsPrec _ "Leaf" = return (Leaf, "")
    readsPrec _ _ = []

data Hop = Hop { name :: String,
                 version :: Int,
                 alpha :: Percentage,
                 amount :: Weight,
                 use :: Use,
                 time :: Duration,
                 notes :: Maybe String,
                 usedAsType :: Maybe UsedAs,
                 form :: Maybe Form,
                 beta :: Maybe Percentage,
                 hSI :: Maybe Percentage,
                 origin :: Maybe String,
                 substitutes :: Maybe String,
                 humulene :: Maybe Percentage,
                 caryophyllene :: Maybe Percentage,
                 cohumulone :: Maybe Percentage,
                 myrcene :: Maybe Percentage
                 }
  deriving (Show, Eq)
