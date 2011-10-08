module Main (main) where

import Test.HUnit (Assertion, assertFailure)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Text.Hastache
import Text.Hastache.Context

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Writer
import Data.Data
import Data.Generics

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [
        testGroup "Lambdas" [
            testCase "Simple Lambda" test_lambdaSection
        ]
    ]

-- Transorm section

test_lambdaSection = do
    res <- hastacheStr defaultConfig (encodeStr template) 
        (mkStrContext context)
    assertEqualStr "result correctness" (decodeStrLBS res) testRes
    where
    template = "\
        \text 1\n\
        \{{#function}}Hello {{human}}!{{/function}}\n\
        \text 2\n\
        \"
    context "human" = MuVariable "You"
    context "function" = MuLambda BS.reverse
    
    testRes = "\
        \text 1\n\
        \!uoY olleH\n\
        \text 2\n\
        \"

test_stub :: Assertion
test_stub = return ()

assertEqualStr preface expected actual =
    unless (actual == expected) (assertFailure msg)
    where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: \n" ++ expected ++ "\nbut got: \n" ++ actual