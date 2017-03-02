import Ingredients
import Transformations

pilsnerMalt = Grain { grainName="Pilsner malt", extractPotential = Efficiency (Percentage 80.0) }
crystalMalt = Grain { grainName="Crystal malt", extractPotential = Efficiency (Percentage 76.0) }

m1 = mkMash (Milliliters 12000)
m2 = addFermentable pilsnerMalt (Grams 5000) m1
m3 = addFermentable crystalMalt (Grams 500) m2
w1 = sparge m3 (Milliliters 20000) (Density 1.050) 

--beer = sparge (addFermentable crystalMalt (Grams 500) (addFermentable pilsnerMalt (Grams 5000) )) 20

main :: IO ()
main = putStrLn ("hej " ++ show w1)