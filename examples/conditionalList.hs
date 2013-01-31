#!/usr/local/bin/runhaskell
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy.Char8 as LZ 

main = hastacheStr defaultConfig (encodeStr template) (mkStrContext context)
    >>= LZ.putStrLn

-- begin example
template = "{{^messages}}No new messages{{/messages}}"
context "messages" = MuList []
