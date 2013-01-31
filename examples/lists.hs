#!/usr/local/bin/runhaskell
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy.Char8 as LZ 

main = hastacheStr defaultConfig (encodeStr template) (mkStrContext context)
    >>= LZ.putStrLn

-- begin example
template = concat [ 
    "{{#heroes}}\n", 
    "* {{name}} \n", 
    "{{/heroes}}\n"] 

context "heroes" = MuList $ map (mkStrContext . mkListContext) 
    ["Nameless","Long Sky","Flying Snow","Broken Sword","Qin Shi Huang"]
    where
    mkListContext name = \"name" -> MuVariable name
