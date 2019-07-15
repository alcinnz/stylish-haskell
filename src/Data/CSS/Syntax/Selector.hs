module Data.CSS.Syntax.Selector(
        Selector(..), SimpleSelector(..), PropertyTest(..),
        parseSelectors
    ) where

import Data.CSS.Syntax.Tokens
import Data.CSS.Syntax.StylishUtil

import Data.Text.Internal (Text(..))

-- type Selector = [SimpleSelector]
data Selector = Element [SimpleSelector] |
    Child Selector [SimpleSelector] | Descendant Selector [SimpleSelector] |
    Adjacent Selector [SimpleSelector] | Sibling Selector [SimpleSelector]
    deriving (Show, Eq)
data SimpleSelector = Tag Text | Id Text | Class Text | Property Text PropertyTest
    deriving (Show, Eq)
data PropertyTest = Exists | Equals Text | Suffix Text | Prefix Text | Substring Text |
    Include Text | Dash Text
    deriving (Show, Eq)

parseSelectors :: Parser [Selector]
parseSelectors tokens = concatP (:) parseCompound parseSelectorsTail $ skipSpace tokens
parseSelectorsTail :: Parser [Selector]
parseSelectorsTail (Comma:tokens) = parseSelectors tokens
parseSelectorsTail tokens = ([], tokens)
parseCompound :: Parser Selector
parseCompound tokens = parseCombinators (Element selector) tokens'
    where (selector, tokens') = parseSelector tokens

parseSelector' :: SimpleSelector -> Parser [SimpleSelector]
parseSelector' op tokens = (op:selector, tokens')
    where (selector, tokens') = parseSelector tokens

parseSelector :: Parser [SimpleSelector]
parseSelector (Delim '*':tokens) = parseSelector tokens
parseSelector (Ident tag:tokens) = parseSelector' (Tag tag) tokens
parseSelector (Hash _ i:tokens) = parseSelector' (Id i) tokens
parseSelector (Delim '.':Ident class_:tokens) = parseSelector' (Class class_) tokens
parseSelector (LeftSquareBracket:Ident prop:tokens) =
        concatP appendPropertySel parsePropertySel parseSelector tokens
    where appendPropertySel test selector = Property prop test : selector
parseSelector tokens = ([], tokens)

parseCombinators' :: Selector -> Parser Selector
parseCombinators' selector tokens = parseCombinators selector' tokens'
    where (selector', tokens') = parseCombinator selector tokens
parseCombinators :: Selector -> Parser Selector
parseCombinators selector (Whitespace:tokens) = parseCombinators' selector tokens
parseCombinators selector tokens@(Delim _:_) = parseCombinators' selector tokens
parseCombinators selector tokens = (selector, tokens)

parseCombinator' :: (Selector -> [SimpleSelector] -> Selector)
                    -> Selector -> Parser Selector
parseCombinator' cb selector tokens = (cb selector selector', tokens')
    where (selector', tokens') = parseSelector $ skipSpace tokens
parseCombinator :: Selector -> [Token] -> (Selector, [Token])
parseCombinator selector (Whitespace:tokens) = parseCombinator selector tokens
parseCombinator selector (Delim '>':tokens) = parseCombinator' Child selector tokens
parseCombinator selector (Delim '~':tokens) = parseCombinator' Sibling selector tokens
parseCombinator selector (Delim '+':tokens) = parseCombinator' Adjacent selector tokens
-- Take special care to avoid adding a trailing Descendant when not needed.
parseCombinator selector tokens@(LeftCurlyBracket:_) = (selector, tokens)
parseCombinator selector tokens@(RightCurlyBracket:_) = (selector, tokens)
parseCombinator selector tokens@(RightSquareBracket:_) = (selector, tokens)
parseCombinator selector tokens@(Comma:_) = (selector, tokens)

parseCombinator selector tokens@(RightParen:_) = (selector, tokens)
parseCombinator selector [] = (selector, [])

parseCombinator selector tokens = parseCombinator' Descendant selector tokens

parsePropertySel :: Parser PropertyTest
parsePropertySel (RightSquareBracket:tokens) = (Exists, tokens)
parsePropertySel (Delim '=':tokens) = parsePropertyVal (Equals) tokens
parsePropertySel (SuffixMatch:tokens) = parsePropertyVal (Suffix) tokens
parsePropertySel (PrefixMatch:tokens) = parsePropertyVal (Prefix) tokens
parsePropertySel (SubstringMatch:tokens) = parsePropertyVal (Substring) tokens
parsePropertySel (IncludeMatch:tokens) = parsePropertyVal (Include) tokens
parsePropertySel (DashMatch:tokens) = parsePropertyVal (Dash) tokens
parsePropertySel tokens = (Exists, skipBlock tokens)

parsePropertyVal :: (Text -> PropertyTest) -> Parser PropertyTest
parsePropertyVal wrapper (Ident val:RightSquareBracket:tokens) = (wrapper val, tokens)
parsePropertyVal wrapper (String val:RightSquareBracket:tokens) = (wrapper val, tokens)
parsePropertyVal _ tokens = (Exists, skipBlock tokens)
