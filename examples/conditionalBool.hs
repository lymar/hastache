#!/usr/local/bin/runhaskell
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.Text.Lazy.IO as TL 

main = hastacheStr defaultConfig (encodeStr template) (mkStrContext context)
    >>= TL.putStrLn

-- begin example
template = "{{#boolean}}true{{/boolean}}{{^boolean}}false{{/boolean}}"
context "boolean" = MuBool False
