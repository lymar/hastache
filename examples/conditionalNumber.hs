#!/usr/local/bin/runhaskell
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy as LZ 

-- begin example
main = mapM_ (\ctx ->
    hastacheStr defaultConfig (encodeStr template) (mkStrContext ctx)
    >>= LZ.putStrLn) [context1,context2]

template = "{{#msg}}{{msg}}{{/msg}}{{^msg}}No{{/msg}} new messages."

context1 "msg" = MuVariable (100 :: Int)
context2 "msg" = MuVariable (0 :: Int)
