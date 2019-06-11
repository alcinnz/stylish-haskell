module Stylish.Parse.Selector(
        Selector(..), SimpleSelector(..), PropertyTest(..),
        parseSelectors
    ) where

import Data.CSS.Syntax.Tokens
import Utils

data Selector = Selector [SimpleSelector]
data SimpleSelector = Tag Text | Id Text | Class Text | Property Text PropertyTest |
    Child | Descendant | Adjacent | Sibling
data PropertyTest = Exists | Equals Text | Suffix Text | Prefix Text | Substring Text |
    Include Text | Dash Text

parseSelectors :: [Token] -> (Selector, [Token])
parseSelectors tokens = concatP (:) (parseSelector) (parseSelectorsTail) skipSpace tokens
parseSelectorsTail Comma:tokens = parseSelectors token
parseSelectorsTail tokens = ([], tokens)

parseSelector' op tokens = (op:selector, tokens')
    where (selector, tokens') = parseSelector tokens

parseSelector Ident tag:tokens = parseSelector' Tag tag tokens
parseSelector Hash _ id:tokens = parseSelector' Id id tokens
parseSelector Delim '.':Ident class_:tokens = parseSelector' Class class_ tokens
parseSelector LeftSquareBracket:Ident prop:tokens =
        concatP (appendPropertySel) (parsePropertySel) (parseSelector) tokens
    where appendPropertySel test selector = (Property prop test):selector

parseSelector Whitespace:tokens = parseCombinator skipSpace tokens
parseSelector Delim c:tokens = parseCombinator Delim c:tokens
parseSelector tokens = ([], tokens)

parseCombinator Delim '>':tokens = parseSelector' Child skipSpace tokens
parseCombinator Delim '~':tokens = parseSelector' Sibling skipSpace tokens
parseCombinator Delim '+':tokens = parseSelector' Adjacent skipSpace tokens
parseCombinator tokens = parseSelector' Descendant tokens

parsePropertySel RightSquareBracket:tokens = (Exists, tokens)
parsePropertySel Delim '=':tokens = parsePropertyVal (Equals) tokens
parsePropertySel SuffixMatch:tokens = parsePropertyVal (Suffix) tokens
parsePropertySel PrefixMatch:tokens = parsePropertyVal (Prefix) tokens
parsePropertySel SubstringMatch:tokens = parsePropertyVal (Substring) tokens
parsePropertySel IncludeMatch:tokens = parsePropertyVal (Include) tokens
parsePropertySel DashMatch:tokens = parsePropertyVal (Dash) tokens
parsePropertySel tokens = (Exists, skipBlock tokens)

parsePropertyVal wrapper Ident val:RightSquareBracket:tokens = (wrapper val, tokens)
parsePropertyVal wrapper String val:RightSquareBracket:tokens = (wrapper val, tokens)
