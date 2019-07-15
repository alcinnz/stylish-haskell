module Data.CSS.Syntax.StyleSheet (
        parse, parse', TrivialStyleSheet(..),
        StyleSheet(..), skipAtRule,
        StyleRule(..),
        -- For parsing at-rules, HTML "style" attribute, etc.
        parseProperties, parseProperties',
        -- for testing
        scanValue
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

addRules :: StyleSheet ss => ss -> ([Selector], [(Text, [Token])]) -> ss
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

parse' :: StyleSheet t => t -> [Token] -> t
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
parseProperties :: Parser [(Text, [Token])]
parseProperties (LeftCurlyBracket:tokens) = parseProperties' tokens
parseProperties (Whitespace:tokens) = parseProperties tokens
-- This error recovery is a bit overly conservative, but it's simple.
parseProperties (_:tokens) = ([], skipAtRule tokens)
parseProperties [] = ([], [])

parseProperties' :: Parser [(Text, [Token])]
parseProperties' (Whitespace:tokens) = parseProperties' tokens
parseProperties' (Ident name:tokens)
    | Colon:tokens' <- skipSpace tokens =
        concatP appendProp scanValue parseProperties' tokens'
    where appendProp value props = (name, value):props
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
skipAtRule (Function _:tokens) = skipAtRule $ skipBlock tokens
skipAtRule (LeftSquareBracket:tokens) = skipAtRule $ skipBlock tokens
-- To ensure parens are balanced, should already be handled.
skipAtRule (RightCurlyBracket:tokens) = RightCurlyBracket:tokens
skipAtRule (RightParen:tokens) = RightParen:tokens
skipAtRule (RightSquareBracket:tokens) = RightSquareBracket:tokens

skipAtRule (_:tokens) = skipAtRule tokens
skipAtRule [] = []

scanValue :: Parser [Token]
scanValue (Semicolon:tokens) = ([], tokens)
scanValue (Whitespace:tokens) = scanValue tokens

scanValue tokens@(LeftCurlyBracket:_) = scanInner tokens scanValue
scanValue tokens@(LeftParen:_) = scanInner tokens scanValue
scanValue tokens@(Function _:_) = scanInner tokens scanValue
scanValue tokens@(LeftSquareBracket:_) = scanInner tokens scanValue
-- To ensure parens are balanced, should already be handled.
scanValue (RightCurlyBracket:tokens) = ([], RightCurlyBracket:tokens)
scanValue (RightParen:tokens) = ([], RightParen:tokens)
scanValue (RightSquareBracket:tokens) = ([], RightSquareBracket:tokens)

scanValue tokens = capture scanValue tokens

skipValue :: [Token] -> [Token]
skipValue tokens = snd $ scanValue tokens
