#!/usr/local/bin/runhaskell
-- | Multiple constructors in generic contexts
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
import           Data.Data
import           Data.Monoid
import           Data.Typeable                 ()

import qualified Data.ByteString.Lazy.Char8 as LZ
import           Text.Hastache
import           Text.Hastache.Context

data Hero = SuperHero { name      :: String
                      , powers    :: [String]
                      , companion :: String
                      }
          | EvilHero  { name   :: String
                      , minion :: String
                      }
          deriving (Show, Data, Typeable)

data SimpleHero = SimpleHero { simpleName :: String }
                deriving (Show, Data, Typeable)

template :: String
template = mconcat [
    "{{#SuperHero}}\n",
    "Hero: {{name}}\n",
    " * Powers: {{#powers}}\n",
    "\n   - {{.}}{{/powers}} \n",
    " * Companion: {{companion}}\n",
    "{{/SuperHero}}\n",
    "{{#EvilHero}}\n",
    "Evil hero: {{name}}\n",
    " * Minion: {{minion}}\n",
    "{{/EvilHero}}"]

render :: Hero -> IO LZ.ByteString
render = hastacheStr defaultConfig (encodeStr template)
       . mkGenericContext

main :: IO ()
main = do let batman = SuperHero "Batman" ["ht","ht"] "Robin"
          let doctorEvil = EvilHero "Doctor Evil" "Mini-Me"
          putStrLn "\n------ batman"
          print batman
          putStrLn "--------------------------"
          render batman     >>= LZ.putStrLn

          putStrLn "\n------ doctorEvil"
          print doctorEvil
          putStrLn "--------------------------"
          render doctorEvil >>= LZ.putStrLn
