#!/usr/local/bin/runhaskell
{-# LANGUAGE DeriveDataTypeable #-}
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy as LZ 
import Data.Data 
import Data.Generics 

-- begin example
main = mapM_ (\(template,context) ->
    hastacheStr defaultConfig (encodeStr template) context >>= LZ.putStrLn) 
        [(template1, mkStrContext context1),
         (template1, context2),
         (template2, context3)]

names = ["Nameless","Long Sky","Flying Snow","Broken Sword","Qin Shi Huang"]

template1 = concat [
    "{{heroes.1.name}}\n",
    "{{heroes.0.name}}\n"]

-- Context as function
context1 "heroes" = MuList $ map (mkStrContext . mkListContext) names
    where
    mkListContext name = \"name" -> MuVariable name
context1 _ = MuNothing

-- With Generics
data Hero = Hero { name :: String } deriving (Data, Typeable)
data Heroes = Heroes { heroes :: [Hero] } deriving (Data, Typeable)

context2 = mkGenericContext $ Heroes $ map Hero names

template2 = concat [
    "{{heroName.3}}\n",
    "{{heroName.2}}\n"]

data HeroesStr = HeroesStr { heroName :: [String] } deriving (Data, Typeable)

context3 = mkGenericContext $ HeroesStr names
