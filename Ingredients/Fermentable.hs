module Ingredients.Fermentable(
Fermentable(..)
) where
import Ingredients.Common

-- Components
data FermentableType = Grain | Sugar | Extract | DryExtract | Adjunct deriving (Show, Eq)
instance Read FermentableType where
    readsPrec _ "Grain" = return (Grain, "")
    readsPrec _ "Sugar" = return (Sugar, "")
    readsPrec _ "Dry Extract" = return (DryExtract, "")
    readsPrec _ "Adjunct" = return (Adjunct, "")
    readsPrec _ _ = []


data Fermentable = Fermentable
            {
                name :: String,
                version :: Int,
                fermentableType :: FermentableType,
                amount :: Weight,
                yield :: Percentage,
                color :: Double,
                addAfterBoil :: Maybe CBool,
                origin :: Maybe String,
                supplier :: Maybe String,
                notes :: Maybe String,
                coarseFineDiff :: Maybe Percentage,
                moisture :: Maybe Percentage,
                diastaticPower :: Maybe Double,
                protein :: Maybe Percentage,
                maxInBatch :: Maybe Percentage,
                recommendMash :: Maybe CBool,
                ibuGalPerLb :: Maybe Double
            }
  deriving (Show, Eq)
