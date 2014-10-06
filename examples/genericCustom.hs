{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Custom extension function for types that are not supported out of
-- the box in generic contexts
import Text.Hastache 
import Text.Hastache.Context 

import qualified Data.Text.Lazy as TL 
import qualified Data.Text.Lazy.IO as TL 

import Data.Data (Data, Typeable)
import Data.Decimal
import Data.Generics.Aliases (extQ)

data DecimalOrInf = Inf | Dec Decimal deriving (Data, Typeable)
deriving instance Data Decimal
data Test = Test {n::Int, m::DecimalOrInf} deriving (Data, Typeable)


val1 :: Test
val1 = Test 1 (Dec $ Decimal 3 1500)

val2 :: Test
val2 = Test 2 Inf

query :: Ext
query = defaultExt `extQ` f
  where f Inf = "+inf"
        f (Dec i) = show i

r "m" = "moo"
r x   = x

example :: Test -> IO TL.Text
example v = hastacheStr defaultConfig
                        (encodeStr template)
                        (mkGenericContext' r query v)

template = concat [ 
     "An int: {{n}}\n",
     "{{#moo.Dec}}A decimal number: {{moo.Dec}}{{/moo.Dec}}",
     "{{#moo.Inf}}An infinity: {{moo.Inf}}{{/moo.Inf}}"
     ] 

main = do
  example val1 >>= TL.putStrLn
  example val2 >>= TL.putStrLn
