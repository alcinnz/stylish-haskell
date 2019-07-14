module Data.CSS.Syntax.StyleSheet (
        parse, TrivialStyleSheet(..),
        StyleSheet(..), skipAtRule,
        StyleRule(..)
    ) where

import Data.CSS.Syntax.Tokens
import Data.CSS.Syntax.Selector
import Data.CSS.Syntax.StylishUtil

import Data.Text.Internal (Text(..))

--------
---- Output type class
--------
class StyleSheet s where
    addRule :: s -> StyleRule -> s
    addAtRule :: s -> Text -> [Token] -> (s, [Token])
    addAtRule self _ tokens = (self, skipAtRule tokens)

addRules self (selector:selectors, properties) = addRules self' (selectors, properties)
    where self' = addRule self $ StyleRule selector properties
addRules self ([], _) = self

data StyleRule = StyleRule Selector [(Text, [Token])] deriving (Show, Eq)

data TrivialStyleSheet = TrivialStyleSheet [StyleRule] deriving (Show, Eq)
instance StyleSheet TrivialStyleSheet where
    addRule (TrivialStyleSheet self) rule = TrivialStyleSheet $ rule:self

--------
---- Basic parsing
--------
parse :: StyleSheet s => s -> Text -> s
parse stylesheet source = parse' stylesheet $ tokenize source

-- Things to skip.
parse' stylesheet (Whitespace:tokens) = parse' stylesheet tokens
parse' stylesheet (CDO:tokens) = parse' stylesheet tokens
parse' stylesheet (CDC:tokens) = parse' stylesheet tokens
parse' stylesheet (Comma:tokens) = parse' stylesheet tokens -- TODO issue warnings.

parse' stylesheet [] = stylesheet

parse' stylesheet (AtKeyword kind:tokens) = parse' stylesheet' tokens'
    where (stylesheet', tokens') = addAtRule stylesheet kind tokens
parse' stylesheet tokens = parse' (addRules stylesheet rule) tokens'
    where (rule, tokens') = concatP (,) parseSelectors parseProperties tokens

--------
---- Property parsing
--------
parseProperties (LeftCurlyBracket:tokens) = parseProperties' tokens
parseProperties (Whitespace:tokens) = parseProperties tokens
-- This error recovery is a bit overly conservative, but it's simple.
parseProperties (_:tokens) = ([], skipAtRule tokens)
parseProperties [] = ([], [])

parseProperties' (Whitespace:tokens) = parseProperties' tokens
parseProperties' (Ident name:tokens)
    | Colon:tokens' <- skipSpace tokens =
        concatP appendProp scanValue parseProperties' tokens'
    where appendProp value tail = (name, value):tail
parseProperties' (RightCurlyBracket:tokens) = ([], tokens)
parseProperties' [] = ([], [])
parseProperties' tokens = parseProperties' (skipValue tokens)

--------
---- Skipping/Scanning utilities
--------
skipAtRule :: [Token] -> [Token]
skipAtRule (Semicolon:tokens) = tokens
skipAtRule (LeftCurlyBracket:tokens) = skipBlock tokens

skipAtRule (LeftParen:tokens) = skipAtRule $ skipBlock tokens
skipAtRule (LeftSquareBracket:tokens) = skipAtRule $ skipBlock tokens
-- To ensure parens are balanced, should already be handled.
skipAtRule (RightCurlyBracket:tokens) = RightCurlyBracket:tokens
skipAtRule (RightParen:tokens) = RightParen:tokens
skipAtRule (RightSquareBracket:tokens) = RightSquareBracket:tokens

skipAtRule (_:tokens) = skipAtRule tokens
skipAtRule [] = []

scanValue (Semicolon:tokens) = ([], tokens)
scanValue (Whitespace:tokens) = scanValue tokens

scanValue (LeftCurlyBracket:tokens) = scanInner tokens scanValue
scanValue (LeftParen:tokens) = scanInner tokens scanValue
scanValue (LeftSquareBracket:tokens) = scanInner tokens scanValue
-- To ensure parens are balanced, should already be handled.
scanValue (RightCurlyBracket:tokens) = ([], RightCurlyBracket:tokens)
scanValue (RightParen:tokens) = ([], RightParen:tokens)
scanValue (RightSquareBracket:tokens) = ([], RightSquareBracket:tokens)

scanValue tokens = capture scanValue tokens

skipValue tokens = snd $ scanValue tokens
