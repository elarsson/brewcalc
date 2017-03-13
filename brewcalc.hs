import Ingredients
import Transformations
import Utilities

pilsnerMalt = Grain { grainName="Pilsner malt", extractPotential = Efficiency (Percentage 80.0) }
crystalMalt = Grain { grainName="Crystal malt", extractPotential = Efficiency (Percentage 76.0) }
saazHops = Hops { hopName = "Saaz", alphaContent = Percentage 4.5 }
perleHops = Hops { hopName = "Perle", alphaContent = Percentage 8.5 }

--mkMash (Milliliters 12000),
mashSteps = [
                addFermentable pilsnerMalt (Grams 5000),
                addFermentable crystalMalt (Grams 500)
            ]

mashIt :: Mash -> [Mash -> Mash] -> Mash            
mashIt m (x:[]) = x m
mashIt m (x:xs) = mashIt (x m) xs

wortIt :: Wort -> [Wort -> Wort] -> Wort
wortIt w (x:[]) w = x w
wortIt w (x:xs) w = wortIt (x w) xs

m1 = wortIt wortSteps $ mashIt (mkMash (Milliliters 12000)) mashSteps
-- m2 =  addFermentable pilsnerMalt (Grams 5000) $ mkMash (Milliliters 12000)
-- m3 = addFermentable crystalMalt (Grams 500) m2
-- w1 = spargeWithEstimate m3 (Milliliters 20000)
-- w2 = addHops w1 saazHops (Grams 30)
-- w3 = boilWithEvaporation w1 (Minutes 45.0)
-- w4 = addHops w3 perleHops (Grams 20)
-- w5 = boilWithEvaporation w4 (Minutes 15.0)
-- w6 = chill w5 (Celsius 18)
-- b1 = ferment w6 (Celsius 18) (Minutes 60*24*7)
--beer = sparge (addFermentable crystalMalt (Grams 500) (addFermentable pilsnerMalt (Grams 5000) )) 20

main :: IO ()
main = putStrLn ("hej " ++ show m1)