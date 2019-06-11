module Stylish.Parse (
        parse,
        skipAtRule,
        StyleSheet, addRule, addAtRule,
        StyleRule,
        Selector(..), SimpleSelector(..), PropertyTest(..)
    ) where

import Data.CSS.Syntax.Tokens
import Stylish.Parse.Selector
import Parse.Utils

--------
---- Output type class
--------
class StyleSheet s where
    addRule :: s -> StyleRule -> s
    addAtRule :: s -> Text -> [Token] -> (s, [Token])
    addAtRule self _ tokens = (self, skipAtRule tokens)

data StyleRule = StyleRule [Selector] [(Text, [Token])]

--------
---- Basic parsing
--------
parse :: StyleSheet s => s -> Text -> s
parse stylesheet source = parse' stylesheet tokenize source

-- Things to skip.
parse' stylesheet Whitespace:tokens = parse' stylesheet tokens
parse' stylesheet CDO:tokens = parse' stylesheet tokens
parse' stylesheet CDC:tokens = parse' stylesheet tokens
parse' stylesheet Comma:tokens = parse' stylesheet tokens -- TODO issue warnings.

parse' stylesheet [] = stylesheet

parse' stylesheet AtKeyword(kind):tokens = parse' stylesheet' tokens'
    where (stylesheet', tokens') = addAtRule stylesheet kind tokens
parse' stylesheet tokens = parse' addRule stylesheet StyleRule selectors properties tokens'
    where (selectors, block) = parseSelectors tokens
        (properties, tokens') = parseProperties block
parse' stylesheet tokens = parse' (addRule stylesheet rule) tokens'
    where (rule, tokens') = concatP (StyleRule) (parseSelector) (parseProperties)

--------
---- Property parsing
--------
parseProperties LeftCurlyBracket:tokens = parseProperties' tokens
parseProperties Whitespace:tokens = parseProperties tokens

parseProperties' Whitespace:tokens = parseProperties' tokens
parseProperties' (Ident name):tokens
    | Colon:tokens' <- skipSpace tokens =
        concatP (appendProp) (scanValue) (parseProperties') tokens'
    where appendProp value tail = (name, value):tail
parseProperties' RightCurlyBracket:tokens = [], tokens
parseProperties' tokens = parseProperties' skipValue tokens

--------
---- Skipping/Scanning utilities
--------
skipAtRule Semicolon:tokens = tokens
skipAtRule LeftCurlyBracket:tokens = skipBlock tokens

skipAtRule LeftParen:tokens = skipAtRule skipBlock tokens
skipAtRule LeftSquareBracket:tokens = skipAtRule skipBlock tokens
-- To ensure parens are balanced, should already be handled.
skipAtRule RightCurlyBracket:tokens = RightCurlyBracket:tokens
skipAtRule RightParen:tokens = RightParen:tokens
skipAtRule RightSquareBracket:tokens = RightSquareBracket:tokens

skipAtRule _:tokens = skipAtRule tokens

scanValue Semicolon:tokens = ([], tokens)
scanValue Whitespace:tokens = scanValue tokens

scanValue LeftCurlyBracket:tokens = scanInner tokens scanValue
scanValue LeftParen:tokens = scanInner tokens scanValue
scanValue LeftSquareBracket:tokens = scanInner tokens scanValue
scanInner tokens cb = concatP (++) (scanBlock) (cb)
-- To ensure parens are balanced, should already be handled.
scanValue RightCurlyBracket:tokens = ([], RightCurlyBracket:tokens)
scanValue RightParen:tokens = ([], RightParen:tokens)
scanValue RightSquareBracket:tokens = ([], RightSquareBracket:tokens)

scanValue tokens = capture scanValue tokens

scanValue tokens = snd scanValue tokens

-- TODO assert closing tags are correct
--    But what should the error recovery be?
scanBlock RightCurlyBracket:tokens = ([RightCurlyBracket], tokens)
scanBlock RightParen:tokens = ([RightParen], tokens)
scanBlock RightSquareBracket:tokens = ([RightSquareBracket], tokens)

scanBlock LeftCurlyBracket:tokens = scanInner tokens scanBlock
scanBlock LeftParen:tokens = scanInner tokens scanBlock
scanBlock RightSquareBracket:tokens = scanInner tokens scanBlock

scanBlock tokens = capture scanBlock tokens

skipBlock tokens = snd scanBlock tokens
