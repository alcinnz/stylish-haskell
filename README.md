# Stylish Haskell
Generic CSS style engine for Haskell, intended to aid the development of new browser engines.

Stylish Haskell implements CSS selection and cascade (but not inheritance) independant of the CSS at-rules and properties understood by the caller. It is intended to ease the development of new browser engines, independant of their output targets.

For more interesting projects see: https://github.io/alcinnz/browser-engine-ganarchy/

## Why Haskell?
No matter what you think about Haskell and other functional languages, there are great reasons to choose it for this project.

The primary reason is that the biggest challenge in implementing a CSS engine is in defining all the various CSS properties, and as such it needs to be trivial to define each individual property. Haskell's pattern matching syntax is perfect for this, and it's laziness is useful.

Though beyond that Haskell makes just as trivial to assemble functions as it does datastructures, which comes in very handy for parsing and interpreting programming languages like CSS selectors.

## API
To parse a CSS stylesheet call `Data.CSS.Syntax.StyleSheet.parse` which returns a variant of the passed in `StyleSheet`. `StyleSheet` is a typeclass specifying methods for parsing at-rules (`parseAtRule`), storing parsed style rules (`addRule`), and optionally setting the stylesheet's priority (`setPriority`).

If these ultimately call down into a `Data.CSS.Syntax.Style.QueryableStyleSheet` you can call `queryRules` to find all matching style rules organized by psuedoelement. Once you have these style rules (typically by specifying a psuedoelement) you can call `cascade'` to resolve them into any instance of `PropertyParser`.

`PropertyParser` allows to declaratively (via Haskell pattern matching) specify how to parse CSS properties, and how they're impacted by CSS inheritance. It has four methods: `longhand` and `shorthand` specify how to parse CSS properties, whilst `temp` and `inherit` specifies what the default values should be.