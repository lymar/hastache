{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad
import Control.Monad.Writer
import Data.Char
import Data.Data
import Data.Generics
import Test.HUnit
import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Text as T

resCorrectness = "result correctness"

-- Hastache comments
commentsTest = do
    res <- hastacheStr defaultConfig (encodeStr template) undefined
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \hello {{! comment #1}} world! \n\
        \hello {{! comment #2 \n\
        \multiline\n\
        \}} world! \n\
        \"
    testRes  = "\
        \hello  world! \n\
        \hello  world! \n\
        \"

-- Variables
variablesTest = do
    res <- hastacheStr defaultConfig (encodeStr template)
        (mkStrContext (return . context))
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \   Char:               [ {{Char}} ]            \n\
        \   Double:             [ {{Double}} ]          \n\
        \   Int:                [ {{Int}} ]             \n\
        \   ByteString:         [ {{ByteString}} ]      \n\
        \   Text:               [ {{Text}} ]            \n\
        \   String:             [ {{String}} ]          \n\
        \   HtmlString:         [ {{HtmlString}} ]      \n\
        \   HtmlStringUnEsc:    [ {{{HtmlString}}} ]    \n\
        \   HtmlStringUnEsc2:   [ {{&HtmlString}} ]     \n\
        \"
    context "Char" = MuVariable 'Й'
    context "Double" = MuVariable (123.45 :: Double)
    context "Int" = MuVariable (5 :: Int)
    context "ByteString" = MuVariable (encodeStr "hello - привет")
    context "Text" = MuVariable (T.pack "hello - привет")
    context "String" = MuVariable "hello - привет"
    context "HtmlString" = MuVariable "<p>text (\\)</p>"
    
    testRes  = "\
        \   Char:               [ Й ]            \n\
        \   Double:             [ 123.45 ]          \n\
        \   Int:                [ 5 ]             \n\
        \   ByteString:         [ hello - привет ]      \n\
        \   Text:               [ hello - привет ]            \n\
        \   String:             [ hello - привет ]          \n\
        \   HtmlString:         [ &lt;p&gt;text (&#92;)&lt;/p&gt; ]      \n\
        \   HtmlStringUnEsc:    [ <p>text (\\)</p> ]    \n\
        \   HtmlStringUnEsc2:   [ <p>text (\\)</p> ]     \n\
        \"

-- Show/hide sections
showHideSectionsTest = do
    res <- hastacheStr defaultConfig (encodeStr template)
        (mkStrContext (return . context))
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \no context : {{^noCtx}}Should render{{/noCtx}}\n\
        \text 1\n\
        \{{#emptyList}}\n\
        \   some text\n\
        \{{/emptyList}}\n\
        \text 2\n\
        \{{^emptyList}}\n\
        \   empty list. {{someval}}\n\
        \{{/emptyList}}\n\
        \inline [{{#emptyList}}txt{{/emptyList}}]\n\
        \{{#emptyString}}no {{someval}}{{/emptyString}}\
        \{{^emptyString}}yes {{someval}}{{/emptyString}}\n\
        \{{#emptyInt}}no {{emptyInt}}{{/emptyInt}}\
        \{{^emptyInt}}yes {{emptyInt}}{{/emptyInt}}\n\
        \{{#emptyDouble}}no {{emptyDouble}}{{/emptyDouble}}\
        \{{^emptyDouble}}yes {{emptyDouble}}{{/emptyDouble}}\n\
        \{{#nonEmptyString}}yes {{nonEmptyString}}{{/nonEmptyString}}\
        \{{^nonEmptyString}}no{{/nonEmptyString}}\n\
        \{{#nonEmptyInt}}yes {{nonEmptyInt}}{{/nonEmptyInt}}\
        \{{^nonEmptyInt}}no{{/nonEmptyInt}}\n\
        \{{#nonEmptyDouble}}yes {{nonEmptyDouble}}{{/nonEmptyDouble}}\
        \{{^nonEmptyDouble}}no{{/nonEmptyDouble}}\n\
        \"
    context "noCtx" = MuNothing
    context "emptyList" = MuList []
    context "someval" = MuVariable (5 :: Int)
    context "emptyString" = MuVariable ""
    context "emptyInt" = MuVariable (0 :: Int)
    context "emptyDouble" = MuVariable (0 :: Double)
    context "nonEmptyString" = MuVariable "some"
    context "nonEmptyInt" = MuVariable (1 :: Int)
    context "nonEmptyDouble" = MuVariable (1 :: Double)
    
    testRes = "\
        \no context : Should render\n\
        \text 1\n\
        \text 2\n\
        \   empty list. 5\n\
        \inline []\n\
        \yes 5\n\
        \yes 0\n\
        \yes 0.0\n\
        \yes some\n\
        \yes 1\n\
        \yes 1.0\n\
        \"

-- Render list
listSectionTest = do
    res <- hastacheStr defaultConfig (encodeStr template)
        (mkStrContext (return . context))
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \text 1\n\
        \{{#section}}\n\
        \ * {{name}} \n\
        \{{/section}}\n\
        \text 2\n\
        \inline {{#section}}[{{name}}]{{/section}}\n\
        \"
    context "section" = MuList $ map nameCtx ["Neo", "Morpheus", "Trinity"]
    nameCtx name = mkStrContext (\"name" -> return $ MuVariable name)

    testRes = "\
        \text 1\n\
        \ * Neo \n\
        \ * Morpheus \n\
        \ * Trinity \n\
        \text 2\n\
        \inline [Neo][Morpheus][Trinity]\n\
        \"

-- Show/hide block according to boolean variable
boolSectionTest = do
    res <- hastacheStr defaultConfig (encodeStr template)
        (mkStrContext (return . context))
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \text 1\n\
        \{{#bool_true}}\n\
        \   true: {{someval}} \n\
        \{{/bool_true}}\n\
        \{{^bool_true}}\n\
        \   true inv: {{someval}} \n\
        \{{/bool_true}}\n\
        \{{#bool_false}}\n\
        \   false: {{someval}} \n\
        \{{/bool_false}}\n\
        \{{^bool_false}}\n\
        \   false inv: {{someval}} \n\
        \{{/bool_false}}\n\
        \text 2\n\
        \"
    context "bool_true" = MuBool True
    context "bool_false" = MuBool False
    context "someval" = MuVariable "val"
    
    testRes = "\
        \text 1\n\
        \   true: val \n\
        \   false inv: val \n\
        \text 2\n\
        \"

-- Transorm section
lambdaSectionTest = do
    res <- hastacheStr defaultConfig (encodeStr template)
        (mkStrContext (return . context))
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \text 1\n\
        \{{#function}}Hello{{/function}}\n\
        \text 2\n\
        \"
    context "function" = MuLambda BS.reverse
    
    testRes = "\
        \text 1\n\
        \olleH\n\
        \text 2\n\
        \"

-- Transform section with monadic function
lambdaMSectionTest = do
    (res, writerState) <- runWriterT monadicFunction
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    assertEqualStr "monad state correctness" (decodeStr writerState)
        testMonad
    where
    monadicFunction = do
        res <- hastacheStr defaultConfig
                             {muTemplateRead = liftIO . muTemplateRead defaultConfig}
                           (encodeStr template)
                           (mkStrContext (return . context))
        return res
    template = "\
        \[{{#mf}}abc{{/mf}}]\n\
        \[{{#mf}}def{{/mf}}]\n\
        \"
    context "mf" = MuLambdaM $ \i -> do
        tell i
        return $ BS.reverse i
    testRes = "\
        \[cba]\n\
        \[fed]\n\
        \"
    testMonad = "abcdef"

-- Change delimiters
setDelimiterTest = do
    res <- hastacheStr defaultConfig (encodeStr template)
        (mkStrContext (return . context))
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \text 1\n\
        \{{someVar}}\n\
        \{{=<% %>=}}\n\
        \<%someVar%>\n\
        \<%={{ }}=%>\n\
        \{{someVar}}\n\
        \text 2\n\
        \"
    context "someVar" = MuVariable "some value"
    
    testRes = "\
        \text 1\n\
        \some value\n\
        \some value\n\
        \some value\n\
        \text 2\n\
        \"

-- Render external (partial) template file
partialsTest = do
    res <- hastacheStr defaultConfig (encodeStr template)
        (mkStrContext (return . context))
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \text 1\n\
        \{{> partFile}}\n\
        \text 2\n\
        \"
    context "name" = MuVariable "Neo"
    
    testRes = "\
        \text 1\n\
        \Hi, Neo!\n\
        \text 2\n\
        \"

data InternalData = InternalData {
    intDataField1   :: String,
    intDataField2   :: Int
    }
    deriving (Data, Typeable)
    
data SomeData = SomeData {
    someDataField1      :: String,
    someDataInternal    :: InternalData,
    someDataList        :: [Int],
    someDataObjList     :: [InternalData],
    someMuLambdaBS      :: BS.ByteString -> BS.ByteString,
    someMuLambdaS       :: String -> String,
    someMuLambdaMBS     :: BS.ByteString -> IO BS.ByteString,
    someMuLambdaMS      :: String -> IO String
    }
    deriving (Data, Typeable)

-- Make hastache context from Data.Data deriving type
genericContextTest = do
    res <- hastacheStr defaultConfig (encodeStr template) 
        (mkGenericContext context)
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \text 1\n\
        \{{someDataField1}} {{someDataInternal.intDataField1}} \n\
        \{{someDataInternal.intDataField2}} \n\
        \{{#someDataInternal}}\n\
        \* {{intDataField1}} {{intDataField2}} \n\
        \{{/someDataInternal}}\n\
        \Simple list:\n\
        \{{#someDataList}}\n\
        \* {{.}} \n\
        \{{/someDataList}}\n\
        \List item by index: {{someDataList.1}} \n\
        \Obj list:\n\
        \{{#someDataObjList}}\n\
        \* {{intDataField1}} : {{intDataField2}} \n\
        \{{/someDataObjList}}\n\
        \{{#someMuLambdaBS}}reverse{{/someMuLambdaBS}}\n\
        \{{#someMuLambdaS}}upper{{/someMuLambdaS}}\n\
        \{{#someMuLambdaMBS}}reverse in IO lambda{{/someMuLambdaMBS}}\n\
        \{{#someMuLambdaMS}}upper in IO lambda{{/someMuLambdaMS}}\n\
        \text 2\n\
        \"
    context = SomeData {
        someDataField1 = "aaa",
        someDataInternal = InternalData { 
            intDataField1 = "zzz", intDataField2 = 100 },
        someDataList = [1,2,3],
        someDataObjList = [InternalData "a" 1, InternalData "b" 2,
            InternalData "c" 3],
        someMuLambdaBS = BS.reverse,
        someMuLambdaS = map toUpper,
        someMuLambdaMBS = return . BS.reverse,
        someMuLambdaMS = return . map toUpper
        }
    
    testRes = "\
        \text 1\n\
        \aaa zzz \n\
        \100 \n\
        \* zzz 100 \n\
        \Simple list:\n\
        \* 1 \n\
        \* 2 \n\
        \* 3 \n\
        \List item by index: 2 \n\
        \Obj list:\n\
        \* a : 1 \n\
        \* b : 2 \n\
        \* c : 3 \n\
        \esrever\n\
        \UPPER\n\
        \adbmal OI ni esrever\n\
        \UPPER IN IO LAMBDA\n\
        \text 2\n\
        \"

-- Up-level context from nested block
nestedContextTest = do
    res <- hastacheStr defaultConfig (encodeStr template)
        (mkStrContext (return . context))
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \{{top}}\n\
        \{{#section}}\n\
        \ * {{val}}. {{top}}\n\
        \{{/section}}\n\
        \"
    context "section" = MuList $ map elemCtx ["elem 1", "elem 2"]
    context "top" = MuVariable "top"
    elemCtx vl = mkStrContext (\v -> return $ case v of
        "val" -> MuVariable vl
        _ -> MuNothing
        )

    testRes = "\
        \top\n\
        \ * elem 1. top\n\
        \ * elem 2. top\n\
        \"

-- Up-level context from nested block (Generic version)
data TopData = TopData {
    topDataTop          :: String,
    topDataItems        :: [NestedData]
    }
    deriving (Data, Typeable)

data NestedData = NestedData {
    nestedDataNested    :: String
    }
    deriving (Data, Typeable)

nestedGenericContextTest = do
    res <- hastacheStr defaultConfig (encodeStr template) context
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \Top variable : {{topDataTop}}\n\
        \{{#topDataItems}}\n\
        \-- Nested section\n\
        \Top variable : {{topDataTop}}\n\
        \Nested variable : {{nestedDataNested}}\n\
        \{{/topDataItems}}\n\
        \"
    context = mkGenericContext $ TopData {
        topDataTop = "TOP",
        topDataItems = [
            NestedData "NESTED_ONE",
            NestedData "NESTED_TWO"]
    }
    testRes = "\
        \Top variable : TOP\n\
        \-- Nested section\n\
        \Top variable : TOP\n\
        \Nested variable : NESTED_ONE\n\
        \-- Nested section\n\
        \Top variable : TOP\n\
        \Nested variable : NESTED_TWO\n\
        \"

-- Accessing array item by index
arrayItemsTest = do
    res <- hastacheStr defaultConfig (encodeStr template)
        (mkStrContext (return . context))
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \{{section.0.name}} {{section.1.name}} {{section.2.name}}\n\
        \{{#flags.0.val}}yes{{/flags.0.val}}\n\
        \{{^flags.1.val}}no{{/flags.0.val}}\n\
        \"
    context "section" = MuList $ map nameCtx ["Neo", "Morpheus", "Trinity"]
    context "flags" = MuList $ map flagCtx [True, False]
    context n = MuNothing
    nameCtx name = mkStrContext (\"name" -> return $ MuVariable name)
    flagCtx val = mkStrContext (\"val" -> return $ MuBool val)

    testRes = "\
        \Neo Morpheus Trinity\n\
        \yes\n\
        \no\n\
        \"

-- Accessing array item by index (generic version)
data ArrayItemTest_Item = ArrayItemTest_Item { 
    name :: String 
    } deriving (Data, Typeable)    

data ArrayItemTest_Container = ArrayItemTest_Container { 
    items       :: [ArrayItemTest_Item],
    itemsStr    :: [String]
    } deriving (Data, Typeable)    

arrayItemsTestGeneric = do
    res <- hastacheStr defaultConfig (encodeStr template) context
    assertEqualStr resCorrectness (decodeStrLBS res) testRes
    where
    template = "\
        \{{items.0.name}} {{items.2.name}}\n\
        \{{itemsStr.0}} {{itemsStr.1}}\n\
        \"
    context = mkGenericContext $ ArrayItemTest_Container {
        items = [ArrayItemTest_Item "Bob", ArrayItemTest_Item "Alice",
                 ArrayItemTest_Item "Zoe"],
        itemsStr = ["Bob", "Alice", "Zoe"]
    }
    testRes = "\
        \Bob Zoe\n\
        \Bob Alice\n\
        \"

tests = TestList [
     TestLabel "Comments test" (TestCase commentsTest)
    ,TestLabel "Variables test" (TestCase variablesTest)
    ,TestLabel "Show/hide sections test" (TestCase showHideSectionsTest)
    ,TestLabel "List test" (TestCase listSectionTest)
    ,TestLabel "Bool test" (TestCase boolSectionTest)
    ,TestLabel "Lambda test" (TestCase lambdaSectionTest)
    ,TestLabel "LambdaM test" (TestCase lambdaMSectionTest)
    ,TestLabel "Set delimiter test" (TestCase setDelimiterTest)
    ,TestLabel "Partials test" (TestCase partialsTest)
    ,TestLabel "Generic context test" (TestCase genericContextTest)
    ,TestLabel "Nested context test" (TestCase nestedContextTest)
    ,TestLabel "Nested generic context test" (TestCase nestedGenericContextTest)
    ,TestLabel "Accessing array item by index" (TestCase arrayItemsTest)
    ,TestLabel "Accessing array item by index (generic version)" 
        (TestCase arrayItemsTestGeneric)
    ]

main = do
    runTestTT tests

assertEqualStr preface actual expected =
    unless (actual == expected) (assertFailure msg)
    where 
    msg = (if null preface then "" else preface ++ "\n") ++
        "expected: \n" ++ expected ++ "\nbut got: \n" ++ actual

