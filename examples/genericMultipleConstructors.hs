#!/usr/local/bin/runhaskell
-- | Multiple constructors in generic contexts
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
import           Data.Data
import           Data.Monoid
import           Data.Typeable                 ()

import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TL
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

render :: Hero -> IO TL.Text
render = hastacheStr defaultConfig (encodeStr template)
       . mkGenericContext

main :: IO ()
main = do let batman = SuperHero "Batman" ["ht","ht"] "Robin"
          let doctorEvil = EvilHero "Doctor Evil" "Mini-Me"
          render batman >>= TL.putStrLn
          render doctorEvil >>= TL.putStrLn
