#!/usr/local/bin/runhaskell
{-# LANGUAGE DeriveDataTypeable #-}
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LZ 
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
    byteStringFunc          :: B.ByteString -> B.ByteString,
    monadicStringFunc       :: String -> IO String,
    monadicByteStringFunc   :: B.ByteString -> IO B.ByteString
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
        "{{#byteStringFunc}}reverse{{/byteStringFunc}}",
        "{{#monadicStringFunc}}upper (monadic){{/monadicStringFunc}}",
        "{{#monadicByteStringFunc}}reverse (monadic){{/monadicByteStringFunc}}"]
    context = Example { stringField = "string value", intField = 1, 
        dataField = InternalData "val" 123, simpleListField = ["a","b","c"],
        dataListField = [InternalData "aaa" 1, InternalData "bbb" 2],
        stringFunc = map toUpper,
        byteStringFunc = B.reverse,
        monadicStringFunc = return . map toUpper,
        monadicByteStringFunc = return . B.reverse }

main = example >>= LZ.putStrLn
