{-# LANGUAGE ExistentialQuantification, FlexibleInstances, FlexibleContexts,
    IncoherentInstances #-}
-- Module:      Text.Hastache
-- Copyright:   Sergey S Lymar (c) 2011 
-- License:     BSD3
-- Maintainer:  Sergey S Lymar <sergey.lymar@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Haskell implementation of Mustache templates

{- | Haskell implementation of Mustache templates

See homepage for examples of usage: <http://github.com/lymar/hastache>
-}
module Text.Hastache (
      hastacheStr
    , hastacheFile
    , MuContext
    , MuType(..)
    , MuConfig(..)
    , MuVar
    , htmlEscape
    , emptyEscape
    , defaultConfig
    , encodeStr
    , encodeStrLBS
    , decodeStr
    , decodeStrLBS
    ) where 

import Prelude hiding (putStrLn, readFile, length, drop, tail, dropWhile, 
    elem, head, last, reverse, take, span)
import Data.ByteString hiding (map, foldl1)
import qualified Data.ByteString.Lazy as LZ
import qualified Codec.Binary.UTF8.String as SU
import Control.Monad.Writer.Lazy (tell, liftIO, MonadIO, execWriterT,
    MonadWriter)
import Data.Char (ord)
import Data.Word
import Control.Monad (guard, when)
import System.FilePath (combine)
import System.Directory (doesFileExist)
import Control.Monad.Trans (lift)
import Data.Int
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Maybe (isJust)

(~>) :: a -> (a -> b) -> b
x ~> f = f $ x
infixl 9 ~>

-- | Data for Hastache variable
type MuContext m = 
    ByteString  -- ^ Variable name
    -> MuType m -- ^ Value

class Show a => MuVar a where
    toLByteString   :: a -> LZ.ByteString
        
instance MuVar ByteString where
    toLByteString = toLBS
    
instance MuVar LZ.ByteString where
    toLByteString = id
    
withShowToLBS a = show a ~> encodeStr ~> toLBS

instance MuVar Integer where toLByteString = withShowToLBS
instance MuVar Int where toLByteString = withShowToLBS
instance MuVar Float where toLByteString = withShowToLBS
instance MuVar Double where toLByteString = withShowToLBS
instance MuVar Int8 where toLByteString = withShowToLBS
instance MuVar Int16 where toLByteString = withShowToLBS
instance MuVar Int32 where toLByteString = withShowToLBS
instance MuVar Int64 where toLByteString = withShowToLBS
instance MuVar Word where toLByteString = withShowToLBS
instance MuVar Word8 where toLByteString = withShowToLBS
instance MuVar Word16 where toLByteString = withShowToLBS
instance MuVar Word32 where toLByteString = withShowToLBS
instance MuVar Word64 where toLByteString = withShowToLBS

instance MuVar Text.Text where 
    toLByteString t = Text.unpack t ~> encodeStr ~> toLBS

instance MuVar LText.Text where 
    toLByteString t = LText.unpack t ~> encodeStr ~> toLBS
    
instance MuVar Char where
    toLByteString a = (a : "") ~> encodeStr ~> toLBS

instance MuVar a => MuVar [a] where
    toLByteString a = (toLByteString '[') <+> cnvLst <+> (toLByteString ']')
        where
        cnvLst = (map toLByteString a) ~> 
                (LZ.intercalate (toLByteString ','))
        (<+>) = LZ.append

instance MuVar [Char] where
    toLByteString k = k ~> encodeStr ~> toLBS
    
data MuType m = 
    forall a. MuVar a => MuVariable a           |
    MuList [MuContext m]                        |
    MuBool Bool                                 |
    MuLambda (ByteString -> ByteString)         |
    MuLambdaM (ByteString -> m ByteString)
    
instance Show (MuType m) where
    show (MuVariable a) = "MuVariable " ++ show a
    show (MuList _) = "MuList [..]"
    show (MuBool v) = "MuBool " ++ show v
    show (MuLambda _) = "MuLambda <..>"
    show (MuLambdaM _) = "MuLambdaM <..>"

data MuConfig = MuConfig {
    muEscapeFunc        :: LZ.ByteString -> LZ.ByteString, 
        -- ^ Escape function ('htmlEscape', 'emptyEscape' etc.)
    muTemplateFileDir   :: Maybe FilePath,
        -- ^ Directory for search partial templates ({{> templateName}})
    muTemplateFileExt   :: Maybe String
        -- ^ Partial template files extension
    }

-- | Convert String to UTF-8 Bytestring
encodeStr :: String -> ByteString
encodeStr = pack . SU.encode

-- | Convert String to UTF-8 Lazy Bytestring
encodeStrLBS :: String -> LZ.ByteString
encodeStrLBS = LZ.pack . SU.encode

-- | Convert UTF-8 Bytestring to String
decodeStr :: ByteString -> String
decodeStr = SU.decode . unpack

-- | Convert UTF-8 Lazy Bytestring to String
decodeStrLBS :: LZ.ByteString -> String
decodeStrLBS = SU.decode . LZ.unpack

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

-- | Escape HTML symbols
htmlEscape :: LZ.ByteString -> LZ.ByteString
htmlEscape str = LZ.unpack str ~> proc ~> LZ.pack
    where
    proc :: [Word8] -> [Word8]
    proc (h:t)
        | h == ord8 '&' = stp "&amp;" t
        | h == ord8 '\\'= stp "&#92;" t
        | h == ord8 '"' = stp "&quot;" t
        | h == ord8 '\''= stp "&#39;" t
        | h == ord8 '<' = stp "&lt;" t
        | h == ord8 '>' = stp "&gt;" t
        | otherwise     = h : (proc t)
    proc [] = []
    stp a t = (map ord8 a) ++ (proc t)

-- | No escape
emptyEscape :: LZ.ByteString -> LZ.ByteString
emptyEscape = id

{- | Default config: HTML escape function, current directory as 
     template directory, template file extension not specified -}
defaultConfig :: MuConfig 
defaultConfig = MuConfig { 
    muEscapeFunc = htmlEscape,
    muTemplateFileDir = Nothing,
    muTemplateFileExt = Nothing
    } 

defOTag = encodeStr "{{"
defCTag = encodeStr "}}"
unquoteCTag = encodeStr "}}}"

findBlock :: ByteString -> ByteString -> ByteString
     -> Maybe (ByteString, Word8, ByteString, ByteString)
findBlock str otag ctag = do
    guard (length fnd > (length otag))
    Just (pre, symb, inTag, afterClose)
    where
    (pre, fnd) = breakSubstring otag str
    symb = index fnd (length otag)
    (inTag, afterClose)
        -- test for unescape ( {{{some}}} )
        | symb == ord8 '{' && ctag == defCTag = 
            breakSubstring unquoteCTag fnd ~> \(a,b) -> 
            (drop (length otag) a, drop 3 b)
        | otherwise = breakSubstring ctag fnd ~> \(a,b) -> 
            (drop (length otag) a, drop (length ctag) b)

toLBS :: ByteString -> LZ.ByteString
toLBS v = LZ.fromChunks [v]

-- | Runtime context   
data MuRtCtx a b = MuRtCtx (MuContext a) (MuContext b)

-- | Try to read variable from both contexts
readVar :: MuRtCtx a b -> ByteString -> LZ.ByteString
readVar (MuRtCtx l g) name =
-- TODO need refactor
    choose lVar gVar
    where
        lVar = extract $ l name
        gVar = extract $ g name
        extract a = case a of 
            MuVariable a -> toLByteString a
            MuBool a -> show a ~> encodeStr ~> toLBS
            _ -> LZ.empty
        choose lv gv 
            | not (LZ.null lv) = lv
            | not (LZ.null gv) = gv
            | otherwise = LZ.empty
        
findCloseSection :: ByteString -> ByteString -> ByteString -> ByteString
     -> Maybe (ByteString, ByteString)
findCloseSection str name otag ctag = do
    guard (length after > 0)
    Just (before, drop (length close) after)
    where
    close = foldl1 append [otag, encodeStr "/", name, ctag]
    (before, after) = breakSubstring close str

trimCharsTest :: Word8 -> Bool
trimCharsTest = (`elem` (encodeStr " \t"))

trimAll :: ByteString -> ByteString
trimAll str = span trimCharsTest str ~> snd ~> spanEnd trimCharsTest ~> fst

tellBS :: (MonadWriter LZ.ByteString m) => ByteString -> m ()
tellBS str = toLBS str ~> tell

processBlock str context otag ctag conf = 
    case findBlock str otag ctag of
        Just (pre, symb, inTag, afterClose) -> do
            tellBS pre
            renderBlock context symb inTag afterClose 
                otag ctag conf
        Nothing -> do
            tellBS str
            return ()

-- TODO need context extraction
renderBlock (MuRtCtx l g) symb inTag afterClose otag ctag conf
    -- comment
    | symb == ord8 '!' = next afterClose
    -- unescape variable
    | symb == ord8 '&' || (symb == ord8 '{' && otag == defOTag) = do
        readVar (MuRtCtx l g) (tail inTag ~> trimAll) ~> tell
        next afterClose
    -- section. inverted section
    | symb == ord8 '#' || symb == ord8 '^' = 
        let 
            normalSection = symb == ord8 '#'
        in do
        case findCloseSection afterClose (tail inTag) otag ctag of
            Nothing -> next afterClose
            Just (sectionContent', afterSection') -> 
                let 
                    dropNL str = 
                        if (length str) > 0 && (head str) == ord8 '\n' 
                           then tail str
                           else str
                    sectionContent = dropNL sectionContent'
                    afterSection = 
                        if ord8 '\n' `elem` sectionContent
                            then dropNL afterSection'
                            else afterSection'
                in do
                case l (tail inTag) of
                    MuList [] -> 
                        if normalSection then do next afterSection
                        else do
                            processBlock sectionContent
                                (MuRtCtx l g) otag ctag conf
                            next afterSection
                    MuList b -> 
                        if normalSection then do
                            mapM_ (\c -> processBlock sectionContent
                                (MuRtCtx c g) otag ctag conf) b
                            next afterSection
                        else do next afterSection
                    MuBool True -> 
                        if normalSection then do
                            processBlock sectionContent
                                (MuRtCtx l g) otag ctag conf
                            next afterSection
                        else do next afterSection
                    MuBool False -> 
                        if normalSection then do next afterSection
                        else do
                            processBlock sectionContent
                                (MuRtCtx l g) otag ctag conf
                            next afterSection
                    MuLambda func -> 
                        if normalSection then do
                            func sectionContent ~> tellBS
                            next afterSection
                        else do next afterSection
                    MuLambdaM func -> 
                        if normalSection then do
                            res <- lift (func sectionContent)
                            tellBS res
                            next afterSection
                        else do next afterSection
                    _ -> next afterSection
    -- set delimiter
    | symb == ord8 '=' = 
        let
            lenInTag = length inTag
            delimitersCommand = take (lenInTag - 1) inTag ~> drop 1
            getDelimiter = do
                guard (lenInTag > 4)
                guard ((index inTag $ lenInTag - 1) == ord8 '=')
                [newOTag,newCTag] <- Just $ split (ord8 ' ') 
                    delimitersCommand
                Just (newOTag, newCTag)
        in do
            case getDelimiter of
                Nothing -> next afterClose
                Just (newOTag, newCTag) -> 
                    processBlock (trim' afterClose) (MuRtCtx l g) 
                        newOTag newCTag conf
    -- partials
    | symb == ord8 '>' =
        let 
            fileName' = tail inTag ~> trimAll
            fileName'' = case muTemplateFileExt conf of
                Nothing -> fileName'
                Just ext -> fileName' `append` (encodeStr ext)
            fileName = decodeStr fileName''
            fullFileName = case muTemplateFileDir conf of
                Nothing -> fileName
                Just path -> combine path fileName
        in do
            fe <- liftIO $ doesFileExist fullFileName
            when fe $ do
                cnt <- liftIO $ readFile fullFileName
                next cnt
            next (trim' afterClose)
    -- variable
    | otherwise = do
        readVar (MuRtCtx l g) (trimAll inTag) ~> muEscapeFunc conf ~> tell
        next afterClose
    where
    next t = processBlock t (MuRtCtx l g) otag ctag conf
    trim' content = 
        dropWhile trimCharsTest content
        ~> \t -> if (length t > 0 && head t == ord8 '\n')
            then tail t else content

-- | Render Hastache template from ByteString
hastacheStr :: (MonadIO m) => 
    MuConfig            -- ^ Configuration
    -> ByteString       -- ^ Template
    -> MuContext m      -- ^ Context
    -> m LZ.ByteString
hastacheStr conf str context = 
-- TODO Need context pack
    execWriterT (processBlock str (MuRtCtx context context) 
            defOTag defCTag conf)

-- | Render Hastache template from file
hastacheFile :: (MonadIO m) => 
    MuConfig            -- ^ Configuration
    -> String           -- ^ Template file name
    -> MuContext m      -- ^ Context
    -> m LZ.ByteString
hastacheFile conf file_name context = do
    str <- liftIO $ readFile file_name
    hastacheStr conf str context

