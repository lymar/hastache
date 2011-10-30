{-# LANGUAGE ScopedTypeVariables #-}
-- Module:      Text.Hastache.Context
-- Copyright:   Sergey S Lymar (c) 2011 
-- License:     BSD3
-- Maintainer:  Sergey S Lymar <sergey.lymar@gmail.com>
-- Stability:   experimental
-- Portability: portable

{- | 
Hastache context helpers
-}
module Text.Hastache.Context (
      mkStrContext
    , mkGenericContext
    ) where 

import Data.Data
import Data.Generics
import Data.Int
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText

import Text.Hastache

x ~> f = f $ x
infixl 9 ~>

-- | Make Hastache context from String -> MuType function
mkStrContext :: Monad m => (String -> MuType m) -> MuContext m
mkStrContext f a = decodeStr a ~> f

{- | 
Make Hastache context from Data.Data deriving type

@
data InternalData = InternalData {
    someField       :: String,
    anotherField    :: Int
    } deriving (Data, Typeable, Show)

data Example = Example {
    stringField     :: String,
    intField        :: Int,
    dataField       :: InternalData,
    simpleListField :: [String],
    dataListField   :: [InternalData],
    stringFunc      :: String -> String,
    byteStringFunc  :: Data.ByteString.ByteString -> Data.ByteString.ByteString
    } deriving (Data, Typeable)

example = hastacheStr defaultConfig (encodeStr template) 
    (mkGenericContext context)
    where
    template = concat $ map (++ \"\\n\") [
        \"string: {{stringField}}\",
        \"int: {{intField}}\",
        \"data: {{dataField.someField}}, {{dataField.anotherField}}\",
        \"data: {{#dataField}}{{someField}}, {{anotherField}}{{/dataField}}\",
        \"simple list: {{#simpleListField}}{{.}} {{/simpleListField}}\",
        \"data list:\",
        \"{{#dataListField}}\",
        \" * {{someField}}, {{anotherField}}. top level var: {{intField}}\",
        \"{{/dataListField}}\",
        \"{{#stringFunc}}upper{{/stringFunc}}\",
        \"{{#byteStringFunc}}reverse{{/byteStringFunc}}\"]
    context = Example { stringField = \"string value\", intField = 1, 
        dataField = InternalData \"val\" 123, simpleListField = [\"a\",\"b\",\"c\"],
        dataListField = [InternalData \"aaa\" 1, InternalData \"bbb\" 2],
        stringFunc = map Data.Char.toUpper,
        byteStringFunc = Data.ByteString.reverse }
@

Result:

@
string: string value 
int: 1 
data: val, 123 
data: val, 123 
simple list: a b c  
data list: 
 * aaa, 1. top level var: 1 
 * bbb, 2. top level var: 1 
UPPER 
esrever 
@
-}
mkGenericContext :: (Monad m, Data a, Typeable1 m) => a -> MuContext m
mkGenericContext val = toGenTemp val ~> convertGenTempToContext
    
data TD m = 
      TSimple (MuType m) 
    | TObj [(String, TD m)] 
    | TList [TD m] 
    | TUnknown
    deriving (Show)

toGenTemp :: (Data a, Monad m, Typeable1 m) => a -> TD m
toGenTemp a = zip fields (gmapQ procField a) ~> TObj
    where
    fields = toConstr a ~> constrFields

procField :: (Data a, Monad m, Typeable1 m) => a -> TD m
procField = 
    obj
    `ext1Q` list
    `extQ` (\(i::String)            -> MuVariable (encodeStr i) ~> TSimple)
    `extQ` (\(i::Char)              -> MuVariable i ~> TSimple)
    `extQ` (\(i::Double)            -> MuVariable i ~> TSimple)
    `extQ` (\(i::Float)             -> MuVariable i ~> TSimple)
    `extQ` (\(i::Int)               -> MuVariable i ~> TSimple)
    `extQ` (\(i::Int8)              -> MuVariable i ~> TSimple)
    `extQ` (\(i::Int16)             -> MuVariable i ~> TSimple)
    `extQ` (\(i::Int32)             -> MuVariable i ~> TSimple)
    `extQ` (\(i::Int64)             -> MuVariable i ~> TSimple)
    `extQ` (\(i::Integer)           -> MuVariable i ~> TSimple)
    `extQ` (\(i::Word)              -> MuVariable i ~> TSimple)
    `extQ` (\(i::Word8)             -> MuVariable i ~> TSimple)
    `extQ` (\(i::Word16)            -> MuVariable i ~> TSimple)
    `extQ` (\(i::Word32)            -> MuVariable i ~> TSimple)
    `extQ` (\(i::Word64)            -> MuVariable i ~> TSimple)
    `extQ` (\(i::BS.ByteString)     -> MuVariable i ~> TSimple)
    `extQ` (\(i::LBS.ByteString)    -> MuVariable i ~> TSimple)
    `extQ` (\(i::Text.Text)         -> MuVariable i ~> TSimple)
    `extQ` (\(i::LText.Text)        -> MuVariable i ~> TSimple)
    `extQ` (\(i::Bool)              -> MuBool i ~> TSimple)
    `extQ` muLambdaBS
    `extQ` muLambdaS
    `extQ` muLambdaMBS
    `extQ` muLambdaMS
    where
    obj a = case dataTypeRep (dataTypeOf a) of
        AlgRep [c] -> toGenTemp a
        _ -> TUnknown
    list a = map procField a ~> TList

    muLambdaMBS :: (BS.ByteString -> m BS.ByteString) -> TD m
    muLambdaMBS f = MuLambdaM f ~> TSimple

    muLambdaMS :: forall m. Monad m => (String -> m String) -> TD m
    muLambdaMS f = MuLambdaM fd ~> TSimple
        where
        fd s = decodeStr s ~> f >>= return . encodeStr

    muLambdaBS :: (BS.ByteString -> BS.ByteString) -> TD m
    muLambdaBS f = MuLambda f ~> TSimple

    muLambdaS :: (String -> String) -> TD m
    muLambdaS f = MuLambda fd ~> TSimple
        where
        fd s = decodeStr s ~> f ~> encodeStr 

convertGenTempToContext :: TD t -> MuContext t
convertGenTempToContext v = mkMap "" Map.empty v ~> mkMapContext
    where
    mkMap name m (TSimple t) = Map.insert (encodeStr name) t m
    mkMap name m (TObj lst) = foldl (foldTObj name) m lst ~>
        Map.insert (encodeStr name) 
        ([foldl (foldTObj "") Map.empty lst ~> mkMapContext] ~> MuList)
    mkMap name m (TList lst) = Map.insert (encodeStr name) 
        (map convertGenTempToContext lst ~> MuList) m
    mkMap _ m _ = m
    
    mkName name newName = if length name > 0 
        then concat [name, ".", newName]
        else newName
    foldTObj name m (fn, fv) = mkMap (mkName name fn) m fv
    
    mkMapContext m a = case Map.lookup a m of
        Nothing -> 
            case a == dotBS of
                True -> 
                    case Map.lookup BS.empty m of
                        Nothing -> MuNothing
                        Just a -> a
                _ -> MuNothing
        Just a -> a

dotBS = encodeStr "."

