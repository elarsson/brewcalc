import BeerXML



main :: IO ()
main = do
    recipeFileContents <- readFile "exampledata/recipes.xml"
    -- hopFileContents <- readFile "exampledata/hops.xml"
    -- fermentableFileContents <- readFile "exampledata/grain.xml"
    -- yeastFileContents <- readFile "exampledata/yeast.xml"
    waterFileContents <- readFile "exampledata/water.xml"
    mashFileContents <- readFile "exampledata/mash.xml"
    -- putStrLn ("hej " ++ show (getHops hopFileContents))
    -- putStrLn ("hej " ++ show (getFermentables fermentableFileContents))
    -- putStrLn ("hej " ++ show (getYeasts yeastFileContents))
    -- putStrLn ("hej " ++ show (getWatersFromXml waterFileContents))
    putStrLn ("hej " ++ show (getMashProfilesFromXml mashFileContents))
    putStrLn ("hej " ++ show (getRecipesFromXml recipeFileContents))
    

