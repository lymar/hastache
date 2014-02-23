#!/usr/local/bin/runhaskell
-- begin example
{-# LANGUAGE FlexibleContexts #-}
import Text.Hastache 
import Text.Hastache.Context
import qualified Data.Text.Lazy as TL 
import qualified Data.Text.Lazy.IO as TL 
import Control.Monad.State 

main = run >>= TL.putStrLn

run = evalStateT stateFunc ""

stateFunc :: StateT String IO TL.Text
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
