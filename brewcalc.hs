import Ingredients
import Transformations

pilsnerMalt = Grain { grainName="Pilsner malt", fermentableContent = Percentage 4.0 }
crystalMalt = Grain { grainName="Crystal malt", fermentableContent = Percentage 5.0 }

main :: IO ()
main = putStrLn "hello"