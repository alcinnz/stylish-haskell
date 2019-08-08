# Stylish Haskell
Generic CSS style engine for Haskell, intended to aid the development of new browser engines.

Stylish Haskell implements CSS selection and cascade (but not inheritance) independant of the CSS at-rules and properties understood by the caller. It is intended to ease the development of new browser engines, independant of their output targets.

For more interesting projects see: https://github.io/alcinnz/browser-engine-ganarchy/

## Versioning
The second major number indicates that more of CSS has been implemented within the existing API. Until then the error recovery rules will ensure as yet invalid CSS won't have any effect.

The first major number indicates any other change to the API, and might break your code.

## API
To parse a CSS stylesheet call `Data.CSS.Syntax.StyleSheet.parse` which returns a variant of the passed in `StyleSheet`. `StyleSheet` is a typeclass specifying methods for parsing at-rules (`parseAtRule`), storing parsed style rules (`addRule`), and optionally setting the stylesheet's priority (`setPriority`).

If these ultimately call down into a `Data.CSS.Syntax.Style.QueryableStyleSheet` you can call `queryRules` to find all matching style rules organized by psuedoelement. Once you have these style rules (typically by specifying a psuedoelement) you can call `cascade'` to resolve them into any instance of `PropertyParser`. To query rules not targetting a psuedoelement, you can either lookup the "" psuedoelement or use the `cascade` shorthand.

`PropertyParser` allows to declaratively (via Haskell pattern matching) specify how to parse CSS properties, and how they're impacted by CSS inheritance. It has four methods: `longhand` and `shorthand` specify how to parse CSS properties, whilst `temp` and `inherit` specifies what the default values should be.
