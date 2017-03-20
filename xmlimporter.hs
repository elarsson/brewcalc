import BeerXML



main :: IO ()
main = do
    fileContents <- readFile "exampledata/hops.xml"
    putStrLn ("hej " ++ show (getHops fileContents))

