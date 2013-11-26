import Ingredients
import Transformations

pilsnerMalt = Fermentable { name="Pilsner malt", fermentableContent=4 }
crystalMalt = Fermentable { name="Crystal malt", fermentableContent=0 }

main :: IO ()
main = putStrLn "hello"