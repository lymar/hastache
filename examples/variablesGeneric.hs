#!/usr/local/bin/runhaskell
-- begin example
{-# LANGUAGE DeriveDataTypeable #-}
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.Text.Lazy.IO as TL 
import Data.Data 
import Data.Generics 

main = hastacheStr defaultConfig (encodeStr template) context
    >>= TL.putStrLn

data Info = Info { 
    name    :: String, 
    unread  :: Int 
    } deriving (Data, Typeable)

template = "Hello, {{name}}!\n\nYou have {{unread}} unread messages."
context = mkGenericContext $ Info "Haskell" 100
