# Stylish Haskell
Generic CSS style engine for Haskell, intended to aid the development of new browser engines.

Stylish Haskell implements CSS selection and cascade (but not inheritance) independant of the CSS at-rules and properties understood by the caller. It is intended to ease the development of new browser engines, independant of their output targets.

For more interesting projects see: https://github.io/alcinnz/browser-engine-ganarchy/

## Versioning
The second major number indicates that more of CSS has been implemented within the existing API. Until then the error recovery rules will ensure as yet invalid CSS won't have any effect.

The first major number indicates any other change to the API, and might break your code.

## API
So far I've only implemented a CSS parser via the function `Stylish.Parse.parse` which returns a variant of the passed in `StyleSheet`. `StyleSheet` is a typeclass implementing the logic for parsing CSS atrules and storing style rules.
