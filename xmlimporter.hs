import BeerXML



main :: IO ()
main = do
    hopFileContents <- readFile "exampledata/hops.xml"
    fermentableFileContents <- readFile "exampledata/grain.xml"
    putStrLn ("hej " ++ show (getHops hopFileContents))
    putStrLn ("hej " ++ show (getFermentables fermentableFileContents))
    

