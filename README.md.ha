# Hastache

Haskell implementation of [Mustache templates](http://mustache.github.com/)

## Installation

    cabal update
    cabal install hastache

## Usage

Read [Mustache documentation](http://mustache.github.com/mustache.5.html) for template syntax.

See [Hastache hackage page](http://hackage.haskell.org/package/hastache).

### Examples

#### Variables

{{#example}}variables{{/example}}

{{#runExample}}variables{{/runExample}}

With Generics

{{#example}}variablesGeneric{{/example}}

#### Lists

{{#example}}lists{{/example}}

{{#runExample}}lists{{/runExample}}

With Generics

{{#example}}listsGeneric{{/example}}

Another Generics version

{{#example}}listsGeneric.2{{/example}}

#### Conditional evaluation

Boolean

{{#example}}conditionalBool{{/example}}
{{#runExample}}conditionalBool{{/runExample}}

List

{{#example}}conditionalList{{/example}}
{{#runExample}}conditionalList{{/runExample}}

Number

{{#example}}conditionalNumber{{/example}}
{{#runExample}}conditionalNumber{{/runExample}}

#### Functions

{{#example}}function{{/example}}

{{#runExample}}function{{/runExample}}

#### Monadic functions

{{#example}}monadicFunction{{/example}}

{{#runExample}}monadicFunction{{/runExample}}

#### Generics big example

{{#example}}genericsBig{{/example}}

{{#runExample}}genericsBig{{/runExample}}

#### More examples

 * [Hastache test](https://github.com/lymar/hastache/blob/master/tests/test.hs)
 * Real world example: [README.md file generator](https://github.com/lymar/hastache/blob/master/mkReadme.hs)
