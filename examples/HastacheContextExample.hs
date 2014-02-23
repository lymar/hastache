#!/usr/local/bin/runhaskell
{-# LANGUAGE DeriveDataTypeable #-}
import Text.Hastache 
import Text.Hastache.Context
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Data 
import Data.Generics 
import Data.Char

data InternalData = InternalData {
    someField       :: String,
    anotherField    :: Int
    } deriving (Data, Typeable, Show)

data Example = Example {
    stringField             :: String,
    intField                :: Int,
    dataField               :: InternalData,
    simpleListField         :: [String],
    dataListField           :: [InternalData],
    stringFunc              :: String -> String,
    textFunc                :: T.Text -> T.Text,
    monadicStringFunc       :: String -> IO String,
    monadicTextFunc         :: T.Text -> IO T.Text
    } deriving (Data, Typeable)

example = hastacheStr defaultConfig (encodeStr template) 
    (mkGenericContext context)
    where
    template = concat $ map (++ "\n") [
        "string: {{stringField}}",
        "int: {{intField}}",
        "data: {{dataField.someField}}, {{dataField.anotherField}}",
        "data: {{#dataField}}{{someField}}, {{anotherField}}{{/dataField}}",
        "simple list: {{#simpleListField}}{{.}} {{/simpleListField}}",
        "data list:",
        "{{#dataListField}}",
        " * {{someField}}, {{anotherField}}. top level var: {{intField}}",
        "{{/dataListField}}",
        "{{#stringFunc}}upper{{/stringFunc}}",
        "{{#textFunc}}reverse{{/textFunc}}",
        "{{#monadicStringFunc}}upper (monadic){{/monadicStringFunc}}",
        "{{#monadicTextFunc}}reverse (monadic){{/monadicTextFunc}}"]
    context = Example { stringField = "string value", intField = 1, 
        dataField = InternalData "val" 123, simpleListField = ["a","b","c"],
        dataListField = [InternalData "aaa" 1, InternalData "bbb" 2],
        stringFunc = map toUpper,
        textFunc = T.reverse,
        monadicStringFunc = return . map toUpper,
        monadicTextFunc = return . T.reverse }

main = example >>= TL.putStrLn
