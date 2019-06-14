module Stylish.Parse.Selector(
        Selector(..), SimpleSelector(..), PropertyTest(..),
        parseSelectors
    ) where

import Data.CSS.Syntax.Tokens
import Stylish.Parse.Utils

import Data.Text.Internal (Text(..))

type Selector = [SimpleSelector]
data SimpleSelector = Tag Text | Id Text | Class Text | Property Text PropertyTest |
    Child | Descendant | Adjacent | Sibling
    deriving (Show, Eq)
data PropertyTest = Exists | Equals Text | Suffix Text | Prefix Text | Substring Text |
    Include Text | Dash Text
    deriving (Show, Eq)

parseSelectors :: [Token] -> ([Selector], [Token])
parseSelectors tokens = concatP (:) parseSelector parseSelectorsTail $ skipSpace tokens
parseSelectorsTail (Comma:tokens) = parseSelectors tokens
parseSelectorsTail tokens = ([], tokens)

parseSelector' op tokens = (op:selector, tokens')
    where (selector, tokens') = parseSelector tokens

parseSelector :: [Token] -> (Selector, [Token])
parseSelector (Delim '*':tokens) = parseSelector tokens
parseSelector (Ident tag:tokens) = parseSelector' (Tag tag) tokens
parseSelector (Hash _ id:tokens) = parseSelector' (Id id) tokens
parseSelector (Delim '.':Ident class_:tokens) = parseSelector' (Class class_) tokens
parseSelector (LeftSquareBracket:Ident prop:tokens) =
        concatP appendPropertySel parsePropertySel parseSelector tokens
    where appendPropertySel test selector = Property prop test : selector

parseSelector (Whitespace:tokens) = parseCombinator $ skipSpace tokens
parseSelector (Delim c:tokens) | c `elem` ">~+" = parseCombinator $ Delim c:tokens
parseSelector tokens = ([], tokens)

parseCombinator (Delim '>':tokens) = parseSelector' Child $ skipSpace tokens
parseCombinator (Delim '~':tokens) = parseSelector' Sibling $ skipSpace tokens
parseCombinator (Delim '+':tokens) = parseSelector' Adjacent $ skipSpace tokens
-- Take special care to avoid adding a trailing Descendant when not needed.
parseCombinator tokens@(LeftCurlyBracket:_) = ([], tokens)
parseCombinator tokens@(RightCurlyBracket:_) = ([], tokens)
parseCombinator tokens@(RightSquareBracket:_) = ([], tokens)
parseCombinator tokens@(Comma:_) = ([], tokens)

parseCombinator tokens@(RightParen:_) = ([], tokens)

parseCombinator tokens = parseSelector' Descendant tokens

parsePropertySel (RightSquareBracket:tokens) = (Exists, tokens)
parsePropertySel (Delim '=':tokens) = parsePropertyVal (Equals) tokens
parsePropertySel (SuffixMatch:tokens) = parsePropertyVal (Suffix) tokens
parsePropertySel (PrefixMatch:tokens) = parsePropertyVal (Prefix) tokens
parsePropertySel (SubstringMatch:tokens) = parsePropertyVal (Substring) tokens
parsePropertySel (IncludeMatch:tokens) = parsePropertyVal (Include) tokens
parsePropertySel (DashMatch:tokens) = parsePropertyVal (Dash) tokens
parsePropertySel tokens = (Exists, skipBlock tokens)

parsePropertyVal wrapper (Ident val:RightSquareBracket:tokens) = (wrapper val, tokens)
parsePropertyVal wrapper (String val:RightSquareBracket:tokens) = (wrapper val, tokens)
