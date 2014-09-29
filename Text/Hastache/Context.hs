{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- Module:      Text.Hastache.Context
-- Copyright:   Sergey S Lymar (c) 2011-2013 
-- License:     BSD3
-- Maintainer:  Sergey S Lymar <sergey.lymar@gmail.com>
-- Stability:   experimental
-- Portability: portable

{- | 
Hastache context helpers
-}
module Text.Hastache.Context (
      mkStrContext
    , mkStrContextM
    , mkGenericContext
    , mkGenericContext'
    ) where 

import Data.Data
import Data.Generics
import Data.Int
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

import Text.Hastache

x ~> f = f $ x
infixl 9 ~>

-- | Make Hastache context from String -> MuType function
mkStrContext :: Monad m => (String -> MuType m) -> MuContext m
mkStrContext f a = decodeStr a ~> f ~> return

-- | Make Hastache context from monadic String -> MuType function
mkStrContextM :: Monad m => (String -> m (MuType m)) -> MuContext m
mkStrContextM f a = decodeStr a ~> f

{- | 
Make Hastache context from Data.Data deriving type

Supported field types:

 * String
 
 * Char
 
 * Double

 * Float

 * Int

 * Int8

 * Int16

 * Int32

 * Int64

 * Integer

 * Word

 * Word8

 * Word16

 * Word32

 * Word64

 * Data.ByteString.ByteString

 * Data.ByteString.Lazy.ByteString
 
 * Data.Text.Text

 * Data.Text.Lazy.Text
 
 * Bool

 * Data.Text.Text -> Data.Text.Text

 * Data.Text.Text -> Data.Text.Lazy.Text

 * Data.Text.Lazy.Text -> Data.Text.Lazy.Text
 
 * Data.ByteString.ByteString -> Data.ByteString.ByteString
 
 * String -> String
 
 * Data.ByteString.ByteString -> Data.ByteString.Lazy.ByteString

 * MonadIO m => Data.Text.Text -> m Data.Text.Text

 * MonadIO m => Data.Text.Text -> m Data.Text.Lazy.Text

 * MonadIO m => Data.Text.Lazy.Text -> m Data.Text.Lazy.Text 

 * MonadIO m => Data.ByteString.ByteString -> m Data.ByteString.ByteString
 
 * MonadIO m => String -> m String
 
 * MonadIO m => Data.ByteString.ByteString -> m Data.ByteString.Lazy.ByteString

Example:

@
import Text.Hastache 
import Text.Hastache.Context
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Data 
import Data.Generics 
import Data.Char

data InternalData = InternalData {
    someField       :: String,
    anotherField    :: Int
    } deriving (Data, Typeable, Show)

data Example = Example {
    stringField             :: String,
    intField                :: Int,
    dataField               :: InternalData,
    simpleListField         :: [String],
    dataListField           :: [InternalData],
    stringFunc              :: String -> String,
    textFunc                :: T.Text -> T.Text,
    monadicStringFunc       :: String -> IO String,
    monadicTextFunc         :: T.Text -> IO T.Text
    } deriving (Data, Typeable)

example = hastacheStr defaultConfig (encodeStr template) 
    (mkGenericContext context)
    where
    template = unlines [
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
        \"{{#textFunc}}reverse{{/textFunc}}\",
        \"{{#monadicStringFunc}}upper (monadic){{/monadicStringFunc}}\",
        \"{{#monadicTextFunc}}reverse (monadic){{/monadicTextFunc}}\"]
    context = Example { stringField = \"string value\", intField = 1, 
        dataField = InternalData \"val\" 123, simpleListField = [\"a\",\"b\",\"c\"],
        dataListField = [InternalData \"aaa\" 1, InternalData \"bbb\" 2],
        stringFunc = map toUpper,
        textFunc = T.reverse,
        monadicStringFunc = return . map toUpper,
        monadicTextFunc = return . T.reverse }

main = example >>= TL.putStrLn
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
UPPER (MONADIC)
)cidanom( esrever
@

Hastache also supports datatypes with multiple constructors:

@
data A = A { str :: String }
       | B { num :: Int }

{{#A}}
A : {{str}}
{{/A}}
{{#B}}
B : {{num}}
{{/B}}
@

-}
                    
#if MIN_VERSION_base(4,7,0)
mkGenericContext :: (Monad m, Data a, Typeable m) => a -> MuContext m
#else
mkGenericContext :: (Monad m, Data a, Typeable1 m) => a -> MuContext m
#endif
mkGenericContext val = toGenTemp id val ~> convertGenTempToContext

-- | Like 'mkGenericContext', but apply the given function to record field names
-- when constructing the context.
#if MIN_VERSION_base(4,7,0)
mkGenericContext' :: (Monad m, Data a, Typeable m) => (String -> String) -> a -> MuContext m
#else
mkGenericContext' :: (Monad m, Data a, Typeable1 m) => (String -> String) -> a -> MuContext m
#endif
mkGenericContext' f val = toGenTemp f val ~> convertGenTempToContext
    
data TD m = 
      TSimple (MuType m) 
    | TObj [(String, TD m)] 
    | TList [TD m] 
    | TUnknown
    deriving (Show)

#if MIN_VERSION_base(4,7,0)
toGenTemp :: (Data a, Monad m, Typeable m) => (String -> String) -> a -> TD m
#else
toGenTemp :: (Data a, Monad m, Typeable1 m) => (String -> String) -> a -> TD m
#endif
toGenTemp f a = TObj $ conName : zip fields (gmapQ (procField f) a)
    where
    fields = toConstr a ~> constrFields ~> map f
    conName = (toConstr a ~> showConstr, MuBool True ~> TSimple)

#if MIN_VERSION_base(4,7,0)
procField :: (Data a, Monad m, Typeable m) => (String -> String) -> a -> TD m
#else
procField :: (Data a, Monad m, Typeable1 m) => (String -> String) -> a -> TD m
#endif
procField f =
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
    `extQ` (\(i::T.Text)            -> MuVariable i ~> TSimple)
    `extQ` (\(i::TL.Text)           -> MuVariable i ~> TSimple)
    `extQ` (\(i::Bool)              -> MuBool i     ~> TSimple)

    `extQ` muLambdaTT
    `extQ` muLambdaTTL
    `extQ` muLambdaTLTL
    `extQ` muLambdaBSBS
    `extQ` muLambdaSS
    `extQ` muLambdaBSLBS
    
    `extQ` muLambdaMTT
    `extQ` muLambdaMTTL
    `extQ` muLambdaMTLTL
    `extQ` muLambdaMBSBS
    `extQ` muLambdaMSS
    `extQ` muLambdaMBSLBS
    
    `ext1Q` muMaybe
    where
    obj a = case dataTypeRep (dataTypeOf a) of
        AlgRep (_:_) -> toGenTemp f a
        _ -> TUnknown
    list a = map (procField f) a ~> TList

    muMaybe Nothing = TSimple MuNothing
    muMaybe (Just a) = TList [procField f a]

    muLambdaTT :: (T.Text -> T.Text) -> TD m
    muLambdaTT f = MuLambda f ~> TSimple

    muLambdaTLTL :: (TL.Text -> TL.Text) -> TD m
    muLambdaTLTL f = MuLambda (f . TL.fromStrict) ~> TSimple

    muLambdaTTL :: (T.Text -> TL.Text) -> TD m
    muLambdaTTL f = MuLambda f ~> TSimple

    muLambdaBSBS :: (BS.ByteString -> BS.ByteString) -> TD m
    muLambdaBSBS f = MuLambda (f . T.encodeUtf8) ~> TSimple

    muLambdaBSLBS :: (BS.ByteString -> LBS.ByteString) -> TD m
    muLambdaBSLBS f = MuLambda (f . T.encodeUtf8) ~> TSimple

    muLambdaSS :: (String -> String) -> TD m
    muLambdaSS f = MuLambda fd ~> TSimple
        where
        fd s = decodeStr s ~> f

    -- monadic

    muLambdaMTT :: (T.Text -> m T.Text) -> TD m
    muLambdaMTT f = MuLambdaM f ~> TSimple

    muLambdaMTLTL :: (TL.Text -> m TL.Text) -> TD m
    muLambdaMTLTL f = MuLambdaM (f . TL.fromStrict) ~> TSimple

    muLambdaMTTL :: (T.Text -> m TL.Text) -> TD m
    muLambdaMTTL f = MuLambdaM f ~> TSimple
        
    muLambdaMBSBS :: (BS.ByteString -> m BS.ByteString) -> TD m
    muLambdaMBSBS f = MuLambdaM (f . T.encodeUtf8) ~> TSimple

    muLambdaMBSLBS :: (BS.ByteString -> m LBS.ByteString) -> TD m
    muLambdaMBSLBS f = MuLambdaM (f . T.encodeUtf8) ~> TSimple

    muLambdaMSS :: (String -> m String) -> TD m
    muLambdaMSS f = MuLambdaM fd ~> TSimple
        where
        fd s = decodeStr s ~> f


convertGenTempToContext :: Monad m => TD m -> MuContext m
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

    mkMapContext m a = return $ case Map.lookup a m of
        Nothing ->
            case a == dotT of
                True -> 
                    case Map.lookup T.empty m of
                        Nothing -> MuNothing
                        Just a -> a
                _ -> MuNothing
        Just a -> a

dotT :: T.Text
dotT = T.singleton '.'
