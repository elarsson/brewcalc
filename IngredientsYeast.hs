module IngredientsYeast (
)
where
import Ingredients


data YeastType = Ale | Lager | Wheat | Wine | Champagne deriving (Show, Eq, Read)
data YeastForm = Liquid | Dry | Slant | Culture deriving (Show, Eq, Read)
data YeastAmount = YeastWeight Weight | YeastVolume Volume (Show, Eq)

data Yeast = Yeast
            {
                name :: String,
                version :: Int,
                yeastType :: YeastType,
                form :: YeastForm,
                amount :: YeastAmount
                -- yield :: Percentage,
                -- color :: Double,
                -- addAfterBoil :: Maybe Bool,
                -- origin :: Maybe String,
                -- supplier :: Maybe String,
                -- notes :: Maybe String,
                -- coarseFineDiff :: Maybe Percentage,
                -- moisture :: Maybe Percentage,
                -- diastaticPower :: Maybe Double,
                -- protein :: Maybe Percentage,
                -- maxInBatch :: Maybe Percentage,
                -- recommendMash :: Maybe Bool,
                -- ibuGalPerLb :: Maybe Double
            }
  deriving (Show, Eq)
