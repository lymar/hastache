#!/usr/local/bin/runhaskell
{-# LANGUAGE DeriveDataTypeable #-}
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.Text.Lazy.IO as TL 
import Data.Data 
import Data.Generics 

main = hastacheStr defaultConfig (encodeStr template) context >>= TL.putStrLn

-- begin example
data Book = Book { 
    title           :: String, 
    publicationYear :: Integer 
    } deriving (Data, Typeable) 
 
data Life = Life { 
    born            :: Integer, 
    died            :: Integer 
    } deriving (Data, Typeable) 
     
data Writer = Writer { 
    name            :: String, 
    life            :: Life, 
    books           :: [Book]
    } deriving (Data, Typeable) 
     
template = concat [ 
    "Name: {{name}} ({{life.born}} - {{life.died}})\n", 
    "{{#life}}\n", 
        "Born: {{born}}\n", 
        "Died: {{died}}\n", 
    "{{/life}}\n", 
    "Bibliography:\n", 
    "{{#books}}\n", 
    "    {{title}} ({{publicationYear}})\n", 
    "{{/books}}\n"
    ]

context = mkGenericContext Writer { 
    name = "Mikhail Bulgakov", 
    life = Life 1891 1940, 
    books = [ 
        Book "Heart of a Dog" 1987, 
        Book "Notes of a country doctor" 1926, 
        Book "The Master and Margarita" 1967]
    }
