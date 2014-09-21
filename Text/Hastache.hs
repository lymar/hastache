{-# LANGUAGE ExistentialQuantification, FlexibleInstances, IncoherentInstances,
             OverloadedStrings #-}
-- Module:      Text.Hastache
-- Copyright:   Sergey S Lymar (c) 2011-2013 
-- License:     BSD3
-- Maintainer:  Sergey S Lymar <sergey.lymar@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Haskell implementation of Mustache templates

{- | Haskell implementation of Mustache templates

See homepage for examples of usage: <http://github.com/lymar/hastache>

Simplest example:

@
import           Text.Hastache
import           Text.Hastache.Context
import qualified Data.Text.Lazy.IO as TL

main = do 
    res <- hastacheStr defaultConfig (encodeStr template)  
        (mkStrContext context) 
    TL.putStrLn res 
  where 
    template = \"Hello, {{name}}!\\n\\nYou have {{unread}} unread messages.\" 
    context \"name\" = MuVariable \"Haskell\"
    context \"unread\" = MuVariable (100 :: Int)
@

Result:

@
Hello, Haskell!

You have 100 unread messages.
@

Using Generics:

@
{&#45;\# LANGUAGE DeriveDataTypeable, OverloadedStrings \#&#45;}

import           Text.Hastache
import           Text.Hastache.Context
import qualified Data.Text.Lazy.IO as TL
import           Data.Data

data Info = Info {
    name    :: String,
    unread  :: Int
    } deriving (Data, Typeable)

main = do
    res <- hastacheStr defaultConfig template
        (mkGenericContext inf)
    TL.putStrLn res
  where
    template = \"Hello, {{name}}!\\n\\nYou have {{unread}} unread messages.\"
    inf = Info \"Haskell\" 100
@

-}
module Text.Hastache (
      hastacheStr
    , hastacheFile
    , hastacheStrBuilder
    , hastacheFileBuilder
    , MuContext
    , MuType(..)
    , MuConfig(..)
    , MuVar(..)
    , composeCtx
    , htmlEscape
    , emptyEscape
    , defaultConfig
    , encodeStr
    , encodeStrLT
    , decodeStr
    , decodeStrLT
    ) where 

import Control.Monad (guard, mplus, mzero, liftM )
import Control.Monad.Reader (ask, runReaderT, MonadReader, ReaderT)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.AEq (AEq,(~==))
import Data.Functor ((<$>))
import Data.Int
import Data.Maybe (isJust)
import Data.Monoid (Monoid, mappend, mempty)
import Data.Text hiding (map, foldl1)
import Data.Text.IO
import Data.Word
import Prelude hiding (putStrLn, readFile, length, drop, tail, dropWhile, elem,
    head, last, reverse, take, span, null)
import System.Directory (doesFileExist)
import System.FilePath (combine)
import Control.Monad.State

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LZ
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Foldable as F
import qualified Prelude

(~>) :: a -> (a -> b) -> b
x ~> f = f x
infixl 9 ~>

-- | Data for Hastache variable
type MuContext m =
    Text            -- ^ Variable name
    -> m (MuType m) -- ^ Value

instance (Monad m) => Monoid (MuContext m) where
    mempty = const $ return MuNothing
    a `mappend` b = \v -> do
        x <- a v
        case x of
            MuNothing -> b v
            _ -> return x

-- | Left-leaning compoistion of contexts. Given contexts @c1@ and
-- @c2@, the behaviour of @(c1 <> c2) x@ is following: if @c1 x@
-- produces 'MuNothing', then the result is @c2 x@. Otherwise the
-- result is @c1 x@. Even if @c1 x@ is 'MuNothing', the monadic
-- effects of @c1@ are still to take place.
composeCtx :: (Monad m) => MuContext m -> MuContext m -> MuContext m
composeCtx = mappend

class Show a => MuVar a where
    -- | Convert to lazy 'Data.Text.Lazy.Text'
    toLText   :: a -> TL.Text
    -- | Is empty variable (empty string, zero number etc.)
    isEmpty   :: a -> Bool 
    isEmpty _ = False

instance MuVar Text where
    toLText = TL.fromStrict
    isEmpty = T.null

instance MuVar TL.Text where
    toLText = id
    isEmpty a = TL.length a == 0

instance MuVar BS.ByteString where
    toLText = TL.fromStrict . T.decodeUtf8
    isEmpty a = BS.length a == 0
    
instance MuVar LZ.ByteString where
    toLText = TL.decodeUtf8
    isEmpty a = LZ.length a == 0

withShowToText :: Show a => a -> TL.Text
withShowToText a = show a ~> TL.pack

numEmpty :: (Num a,AEq a) => a -> Bool
numEmpty a = a ~== 0

instance MuVar Integer where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar Int     where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar Float   where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar Double  where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar Int8    where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar Int16   where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar Int32   where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar Int64   where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar Word    where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar Word8   where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar Word16  where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar Word32  where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar Word64  where {toLText = withShowToText; isEmpty = numEmpty}
instance MuVar ()      where {toLText = withShowToText}

    
instance MuVar Char where
    toLText = TL.singleton

instance MuVar a => MuVar [a] where
    toLText a = toLText '[' <+> cnvLst <+> toLText ']'
        where
        cnvLst = map toLText a ~> 
                TL.intercalate (toLText ',')
        (<+>) = TL.append


instance MuVar a => MuVar (Maybe a) where
    toLText (Just a) = toLText a
    toLText Nothing  = ""
    isEmpty Nothing  = True
    isEmpty (Just a) = isEmpty a

instance (MuVar a, MuVar b) => MuVar (Either a b) where
    toLText (Left  a) = toLText a
    toLText (Right b) = toLText b
    isEmpty (Left  a) = isEmpty a
    isEmpty (Right b) = isEmpty b
    

instance MuVar [Char] where
    toLText = TL.pack
    isEmpty a = Prelude.length a == 0

data MuType m = 
    forall a. MuVar a => MuVariable a                   |
    MuList [MuContext m]                                |
    MuBool Bool                                         |
    forall a. MuVar a => MuLambda (Text -> a)           |
    forall a. MuVar a => MuLambdaM (Text -> m a)        |
    MuNothing

instance Show (MuType m) where
    show (MuVariable a) = "MuVariable " ++ show a
    show (MuList _) = "MuList [..]"
    show (MuBool v) = "MuBool " ++ show v
    show (MuLambda _) = "MuLambda <..>"
    show (MuLambdaM _) = "MuLambdaM <..>"
    show MuNothing = "MuNothing"

data MuConfig m = MuConfig {
    muEscapeFunc        :: TL.Text -> TL.Text, 
        -- ^ Escape function ('htmlEscape', 'emptyEscape' etc.)
    muTemplateFileDir   :: Maybe FilePath,
        -- ^ Directory for search partial templates (@{{> templateName}}@)
    muTemplateFileExt   :: Maybe String,
        -- ^ Partial template files extension
    muTemplateRead      :: FilePath -> m (Maybe Text)
        -- ^ Template retrieval function. 'Nothing' indicates that the
        --   template could not be found.
    }

-- | Convert 'String' to 'Text'
encodeStr :: String -> Text
encodeStr = T.pack

-- | Convert 'String' to Lazy 'Data.Text.Lazy.Text'
encodeStrLT :: String -> TL.Text
encodeStrLT = TL.pack

-- | Convert 'Text' to 'String'
decodeStr :: Text -> String
decodeStr = T.unpack

-- | Convert Lazy 'Data.Text.Lazy.Text' to 'String'
decodeStrLT :: TL.Text -> String
decodeStrLT = TL.unpack

-- | isMuNothing x = x == MuNothing
isMuNothing :: MuType t -> Bool
isMuNothing MuNothing = True
isMuNothing _ = False

-- | Escape HTML symbols
htmlEscape :: TL.Text -> TL.Text
htmlEscape = TL.concatMap proc
  where
    proc '&'  = "&amp;"
    proc '\\' = "&#92;"
    proc '"'  = "&quot;"
    proc '\'' = "&#39;"
    proc '<'  = "&lt;"
    proc '>'  = "&gt;"
    proc h    = TL.singleton h

-- | No escape
emptyEscape :: TL.Text -> TL.Text
emptyEscape = id

{- | Default config: HTML escape function, current directory as 
     template directory, template file extension not specified -}
defaultConfig :: MonadIO m => MuConfig m
defaultConfig = MuConfig {
    muEscapeFunc = htmlEscape,
    muTemplateFileDir = Nothing,
    muTemplateFileExt = Nothing,
    muTemplateRead = liftIO . defaultTemplateRead
    }

defaultTemplateRead :: FilePath -> IO (Maybe Text)
defaultTemplateRead fullFileName = do
    fe <- doesFileExist fullFileName
    if fe
        then Just <$> readFile fullFileName
        else return Nothing

defOTag = "{{" :: Text
defCTag = "}}" :: Text
unquoteCTag = "}}}" :: Text

findBlock :: 
       Text 
    -> Text 
    -> Text
    -> Maybe (Text, Char, Text, Text)
findBlock str otag ctag = do
    guard (length fnd > length otag)
    Just (pre, symb, inTag, afterClose)
    where
    (pre, fnd) = breakOn otag str
    symb = index fnd (length otag)
    (inTag, afterClose)
        -- test for unescape ( {{{some}}} )
        | symb == '{' && ctag == defCTag = 
            breakOn unquoteCTag fnd ~> \(a,b) -> 
            (drop (length otag) a, drop 3 b)
        | otherwise = breakOn ctag fnd ~> \(a,b) -> 
            (drop (length otag) a, drop (length ctag) b)

readVar :: Monad m => [MuContext m] -> Text -> m TL.Text
readVar [] _ = return TL.empty
readVar (context:parentCtx) name = do
    muType <- context name
    case muType of
        MuVariable a -> return $ toLText a
        MuBool a -> return . withShowToText $ a
        MuNothing -> do
          mb <- runMaybeT $ tryFindArrayItem context name
          case mb of
            Just (nctx,nn) -> readVar [nctx] nn
            _ -> readVar parentCtx name
        _ -> return TL.empty

readInt :: Text -> Maybe (Int,Text)
readInt t = eitherMaybe $ T.decimal t
  where
    eitherMaybe (Left _)  = Nothing
    eitherMaybe (Right x) = Just x

tryFindArrayItem :: Monad m => 
       MuContext m
    -> Text
    -> MaybeT m (MuContext m, Text)
tryFindArrayItem context name = do
    guard $ length idx > 1
    (idx,nxt) <- MaybeT $ return $ readInt $ tail idx
    guard $ idx >= 0
    guard $ (null nxt) || (head nxt == '.')
    muType <- lift $ context nm
    case muType of
        MuList l -> do
            guard $ idx < (List.length l)
            let ncxt = l !! idx
            if null nxt
                then return (ncxt, dotStr) -- {{some.0}}
                else return (ncxt, tail nxt) -- {{some.0.field}}
        _ -> mzero
    where
    (nm,idx) = breakOn dotStr name
    dotStr = "."

findCloseSection :: 
       Text 
    -> Text 
    -> Text 
    -> Text
    -> Maybe (Text, Text)
findCloseSection str name otag ctag = do
    guard (length after > 0)
    Just (before, drop (length close) after)
    where
    close = foldl1 append [otag, "/", name, ctag]
    (before, after) = breakOn close str

trimCharsTest :: Char -> Bool
trimCharsTest = (`Prelude.elem` " \t")

trimAll :: Text -> Text
trimAll = dropAround trimCharsTest

addRes :: Monad m => (Either T.Text TL.Text) -> StateT TLB.Builder m ()
addRes str = do
    modify (flip mappend t)
  where
    t = either TLB.fromText TLB.fromLazyText str

addResT :: Monad m => T.Text -> StateT TLB.Builder m ()
addResT = addRes . Left

addResTL :: Monad m => TL.Text -> StateT TLB.Builder m ()
addResTL = addRes . Right

processBlock :: Monad m => 
       Text 
    -> [MuContext m] 
    -> Text 
    -> Text 
    -> MuConfig m
    -> StateT TLB.Builder m ()
processBlock str contexts otag ctag conf = 
    case findBlock str otag ctag of
        Just (pre, symb, inTag, afterClose) -> do
            addResT pre
            renderBlock contexts symb inTag afterClose otag ctag conf
        Nothing -> do
            addResT str
            return ()

elem :: Char -> Text -> Bool
elem c = isJust . find (==c)

renderBlock :: Monad m =>
       [MuContext m] 
    -> Char 
    -> Text 
    -> Text 
    -> Text
    -> Text 
    -> MuConfig m
    -> StateT TLB.Builder m ()
renderBlock contexts symb inTag afterClose otag ctag conf
    -- comment
    | symb == '!' = next afterClose
    -- unescape variable
    | symb == '&' || (symb == '{' && otag == defOTag) = do
        addResTL =<< lift (readVar contexts (tail inTag ~> trimAll))
        next afterClose
    -- section, inverted section
    | symb == '#' || symb == '^' = 
        case findCloseSection afterClose (tail inTag) otag ctag of
            Nothing -> next afterClose
            Just (sectionContent', afterSection') -> 
                let 
                    dropNL str = 
                        if length str > 0 && head str == '\n' 
                           then tail str
                           else str
                    sectionContent = dropNL sectionContent'
                    afterSection = 
                        if '\n' `elem` sectionContent
                            then dropNL afterSection'
                            else afterSection'
                    tlInTag = tail inTag
                    readContext' = MaybeT $ liftM (List.find (not . isMuNothing)) $
                                     mapM ($ tlInTag) contexts
                    readContextWithIdx = do
                      (ctx,name) <- Prelude.foldr mplus mzero $
                                    map (\c -> tryFindArrayItem c tlInTag) contexts
                      lift $ ctx name
                    readContext = readContext' `mplus` readContextWithIdx
                    processAndNext = do
                        processBlock sectionContent contexts otag ctag conf
                        next afterSection
                in do
                mbCtx <- lift $ runMaybeT readContext
                if symb == '#'
                    then
                      case mbCtx of -- section
                        Just (MuList []) -> next afterSection
                        Just (MuList b) -> do
                            mapM_ (\c -> processBlock sectionContent
                                (c:contexts) otag ctag conf) b
                            next afterSection
                        Just (MuVariable a) -> if isEmpty a 
                            then next afterSection
                            else processAndNext
                        Just (MuBool True) -> processAndNext
                        Just (MuLambda func) -> do
                            func sectionContent ~> toLText ~> addResTL
                            next afterSection
                        Just (MuLambdaM func) -> do
                            res <- lift (func sectionContent)
                            res ~> toLText ~> addResTL
                            next afterSection
                        _ -> next afterSection
                    else case mbCtx of -- inverted section
                        Just (MuList []) -> processAndNext
                        Just (MuBool False) -> processAndNext
                        Just (MuVariable a) -> if isEmpty a 
                            then processAndNext
                            else next afterSection
                        Nothing -> processAndNext
                        _ -> next afterSection
    -- set delimiter
    | symb == '=' = 
        let
            lenInTag = length inTag
            delimitersCommand = take (lenInTag - 1) inTag ~> drop 1
            getDelimiter = do
                guard $ lenInTag > 4
                guard $ index inTag (lenInTag - 1) == '='
                [newOTag,newCTag] <- Just $ splitOn (singleton ' ') delimitersCommand
                Just (newOTag, newCTag)
        in case getDelimiter of
                Nothing -> next afterClose
                Just (newOTag, newCTag) -> 
                    processBlock (trim' afterClose) contexts
                        newOTag newCTag conf
    -- partials
    | symb == '>' =
        let 
            fileName' = tail inTag ~> trimAll
            fileName'' = case muTemplateFileExt conf of
                Nothing -> fileName'
                Just ext -> fileName' `append` encodeStr ext
            fileName = decodeStr fileName''
            fullFileName = case muTemplateFileDir conf of
                Nothing -> fileName
                Just path -> combine path fileName
        in do
            F.mapM_ next =<< lift (muTemplateRead conf fullFileName)
            next (trim' afterClose)
    -- variable
    | otherwise = do
        addResTL . muEscapeFunc conf =<<
          lift (readVar contexts $ trimAll inTag)
        next afterClose
    where
    next t = processBlock t contexts otag ctag conf
    trim' content = 
        dropWhile trimCharsTest content
        ~> \t -> if length t > 0 && head t == '\n'
            then tail t else content

-- | Render Hastache template from 'Text'
hastacheStr :: (Monad m) => 
       MuConfig m       -- ^ Configuration
    -> Text             -- ^ Template
    -> MuContext m      -- ^ Context
    -> m TL.Text
hastacheStr conf str context = 
    hastacheStrBuilder conf str context >>= return . TLB.toLazyText

-- | Render Hastache template from file
hastacheFile :: (MonadIO m) => 
       MuConfig m       -- ^ Configuration
    -> FilePath         -- ^ Template file name
    -> MuContext m      -- ^ Context
    -> m TL.Text
hastacheFile conf file_name context = 
    hastacheFileBuilder conf file_name context >>= return . TLB.toLazyText

-- | Render Hastache template from 'Text'
hastacheStrBuilder :: (Monad m) => 
       MuConfig m       -- ^ Configuration
    -> Text             -- ^ Template
    -> MuContext m      -- ^ Context
    -> m TLB.Builder
hastacheStrBuilder conf str context =
    execStateT (processBlock str [context] defOTag defCTag conf) mempty

-- | Render Hastache template from file
hastacheFileBuilder :: (MonadIO m) => 
       MuConfig m       -- ^ Configuration
    -> FilePath         -- ^ Template file name
    -> MuContext m      -- ^ Context
    -> m TLB.Builder
hastacheFileBuilder conf file_name context = do
    str <- readFile file_name ~> liftIO
    hastacheStrBuilder conf str context

