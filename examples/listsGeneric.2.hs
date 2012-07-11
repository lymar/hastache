#!/usr/local/bin/runhaskell
{-# LANGUAGE DeriveDataTypeable #-}
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy as LZ 
import Data.Data 
import Data.Generics 

main = hastacheStr defaultConfig (encodeStr template) context
    >>= LZ.putStrLn

-- begin example
data Heroes = Heroes { heroes :: [String] } deriving (Data, Typeable)

template = concat [ 
    "{{#heroes}}\n", 
    "* {{.}} \n", 
    "{{/heroes}}\n"] 

context = mkGenericContext $ Heroes ["Nameless","Long Sky","Flying Snow", 
    "Broken Sword","Qin Shi Huang"]
