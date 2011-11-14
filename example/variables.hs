#!/usr/local/bin/runhaskell
-- begin example
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy as LZ 

main = hastacheStr defaultConfig (encodeStr template) (mkStrContext context)
    >>= LZ.putStrLn

template = "Hello, {{name}}!\n\nYou have {{unread}} unread messages." 

context "name" = MuVariable "Haskell"
context "unread" = MuVariable (100 :: Int)
