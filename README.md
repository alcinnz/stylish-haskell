# Stylish Haskell
Generic CSS style engine for Haskell, intended to aid the development of new browser engines.

Stylish Haskell implements CSS selection and cascade (but not inheritance) independant of the CSS at-rules and properties understood by the caller. It is intended to ease the development of new browser engines, independant of their output targets.

For more interesting projects see: https://github.io/alcinnz/browser-engine-ganarchy/

## Why Haskell?
No matter what you think about Haskell and other functional languages, there are great reasons to choose it for this project.

The primary reason is that the biggest challenge in implementing a CSS engine is in defining all the various CSS properties, and as such it needs to be trivial to define each individual property. Haskell's pattern matching syntax is perfect for this, and it's laziness is useful.

Though beyond that Haskell makes just as trivial to assemble functions as it does datastructures, which comes in very handy for parsing and interpreting programming languages like CSS selectors.

## API
So far I've only implemented a CSS parser via the function `Stylish.Parse.parse` which returns a variant of the passed in `StyleSheet`. `StyleSheet` is a typeclass implementing the logic for parsing CSS atrules and storing style rules.
