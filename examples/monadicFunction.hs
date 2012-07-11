#!/usr/local/bin/runhaskell
-- begin example
{-# LANGUAGE FlexibleContexts #-}
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy as LZ 
import Control.Monad.State 

main = run >>= LZ.putStrLn

run = evalStateT stateFunc ""

stateFunc :: StateT String IO LZ.ByteString
stateFunc = 
    hastacheStr defaultConfig (encodeStr template) (mkStrContext context) 

template = "{{#arg}}aaa{{/arg}} {{#arg}}bbb{{/arg}} {{#arg}}ccc{{/arg}}"

context "arg" = MuLambdaM $ arg . decodeStr

arg :: MonadState String m => String -> m String
arg a = do    
    v <- get
    let nv = v ++ a
    put nv
    return nv
