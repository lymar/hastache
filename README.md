# Hastache

Haskell implementation of [Mustache templates](http://mustache.github.com/)

## Installation

    cabal update
    cabal install hastache

## Usage

Read [Mustache documentation](http://mustache.github.com/mustache.5.html) for template syntax.

### Examples

#### Variables

```haskell
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy as LZ 

main = hastacheStr defaultConfig (encodeStr template) (mkStrContext context)
    >>= LZ.putStrLn

template = "Hello, {{name}}!\n\nYou have {{unread}} unread messages." 

context "name" = MuVariable "Haskell"
context "unread" = MuVariable (100 :: Int)
```

```
Hello, Haskell!

You have 100 unread messages.
```

With Generics

```haskell
{-# LANGUAGE DeriveDataTypeable #-}
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy as LZ 
import Data.Data 
import Data.Generics 

main = hastacheStr defaultConfig (encodeStr template) context
    >>= LZ.putStrLn

data Info = Info { 
    name    :: String, 
    unread  :: Int 
    } deriving (Data, Typeable)

template = "Hello, {{name}}!\n\nYou have {{unread}} unread messages."
context = mkGenericContext $ Info "Haskell" 100
```

#### Lists

```haskell
template = concat [ 
    "{{#heroes}}\n", 
    "* {{name}} \n", 
    "{{/heroes}}\n"] 

context "heroes" = MuList $ map (mkStrContext . mkListContext) 
    ["Nameless","Long Sky","Flying Snow","Broken Sword","Qin Shi Huang"]
    where
    mkListContext name = \"name" -> MuVariable name
```

```
* Nameless 
* Long Sky 
* Flying Snow 
* Broken Sword 
* Qin Shi Huang
```

With Generics

```haskell
data Hero = Hero { name :: String } deriving (Data, Typeable)
data Heroes = Heroes { heroes :: [Hero] } deriving (Data, Typeable)

template = concat [ 
    "{{#heroes}}\n", 
    "* {{name}} \n", 
    "{{/heroes}}\n"] 

context = mkGenericContext $ Heroes $ map Hero ["Nameless","Long Sky",
    "Flying Snow","Broken Sword","Qin Shi Huang"]
```

Another Generics version

```haskell
data Heroes = Heroes { heroes :: [String] } deriving (Data, Typeable)

template = concat [ 
    "{{#heroes}}\n", 
    "* {{.}} \n", 
    "{{/heroes}}\n"] 

context = mkGenericContext $ Heroes ["Nameless","Long Sky","Flying Snow", 
    "Broken Sword","Qin Shi Huang"]
```

#### Conditional evaluation

Boolean

```haskell
template = "{{#boolean}}true{{/boolean}}{{^boolean}}false{{/boolean}}"
context "boolean" = MuBool False
```
```
false
```

List

```haskell
template = "{{^messages}}No new messages{{/messages}}"
context "messages" = MuList []
```
```
No new messages
```

Number

```haskell
main = mapM_ (\ctx ->
    hastacheStr defaultConfig (encodeStr template) (mkStrContext ctx)
    >>= LZ.putStrLn) [context1,context2]

template = "{{#msg}}{{msg}}{{/msg}}{{^msg}}No{{/msg}} new messages."

context1 "msg" = MuVariable (100 :: Int)
context2 "msg" = MuVariable (0 :: Int)
```
```
100 new messages.
No new messages.
```

#### Functions

```haskell
template = "Hello, {{#reverse}}world{{/reverse}}!" 

context "reverse" = MuLambda (reverse . decodeStr)
```

```
Hello, dlrow!
```

#### Monadic functions

```haskell
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
```

```
aaa aaabbb aaabbbccc
```

#### Generics big example

```haskell
data Book = Book { 
    title           :: String, 
    publicationYear :: Integer 
    } deriving (Data, Typeable) 
 
data Life = Life { 
    born            :: Integer, 
    died            :: Integer 
    } deriving (Data, Typeable) 
     
data Writer = Writer { 
    name            :: String, 
    life            :: Life, 
    books           :: [Book]
    } deriving (Data, Typeable) 
     
template = concat [ 
    "Name: {{name}} ({{life.born}} - {{life.died}})\n", 
    "{{#life}}\n", 
        "Born: {{born}}\n", 
        "Died: {{died}}\n", 
    "{{/life}}\n", 
    "Bibliography:\n", 
    "{{#books}}\n", 
    "    {{title}} ({{publicationYear}})\n", 
    "{{/books}}\n"
    ]

context = mkGenericContext Writer { 
    name = "Mikhail Bulgakov", 
    life = Life 1891 1940, 
    books = [ 
        Book "Heart of a Dog" 1987, 
        Book "Notes of a country doctor" 1926, 
        Book "The Master and Margarita" 1967]
    }
```

```
Name: Mikhail Bulgakov (1891 - 1940)
Born: 1891
Died: 1940
Bibliography:
    Heart of a Dog (1987)
    Notes of a country doctor (1926)
    The Master and Margarita (1967)
```

#### More examples

 * [Hastache test](https://github.com/lymar/hastache/blob/master/tests/test.hs)
 * Real world example: [README.md file generator](https://github.com/lymar/hastache/blob/master/mkReadme.hs)
