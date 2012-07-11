#!/usr/local/bin/runhaskell
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy as LZ 
import System.Process (readProcess)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import System.FilePath ((</>))
import Data.List (span, lines, intersperse)
import Data.Char (isSpace)

main = do 
    res <- hastacheFile defaultConfig "README.md.ha"
        (mkStrContext context)
    LZ.writeFile "README.md" res 
    where 
    context "example" = MuLambdaM $ \fn -> do
        cd <- setExampleDir
        fc <- readFile $ exampleFile fn
        let { forTC = case span (\t -> trim t /= be) (lines $ trim fc) of
            (a,[]) -> a
            (_,a) -> drop 1 a }
        let explText = concat $ intersperse "\n" forTC
        
        setCurrentDirectory cd
        return $ concat [
              "```haskell\n"
            , explText
            , "\n```"
            ]

    context "runExample" = MuLambdaM $ \fn -> do
        cd <- setExampleDir
        explRes <- readProcess "runhaskell" [exampleFile fn] []
        setCurrentDirectory cd
        return $ concat [
              "```\n"
            , trim explRes
            , "\n```"
            ]

    be = "-- begin example"
    setExampleDir = do
        cd <- getCurrentDirectory
        setCurrentDirectory $ cd </> "examples"
        return cd
    exampleFile fn = decodeStr fn ++ ".hs"

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace
