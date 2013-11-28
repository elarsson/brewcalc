import Ingredients
import Transformations

pilsnerMalt = Grain { name="Pilsner malt", fermentableContent=4 }
crystalMalt = Grain { name="Crystal malt", fermentableContent=0 }

main :: IO ()
main = putStrLn "hello"