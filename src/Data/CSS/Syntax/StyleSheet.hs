{-# LANGUAGE OverloadedStrings #-}
-- | Parses a CSS stylesheet
-- See `StyleSheet` & `parseForURL`.
module Data.CSS.Syntax.StyleSheet (
        parse, parse', parseForURL, TrivialStyleSheet(..),
        StyleSheet(..), skipAtRule, scanAtRule, scanBlock, skipSpace,
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
import Data.Text (pack, unpack)
import Network.URI (parseRelativeReference, relativeTo, uriToString, URI(..))

--------
---- Output type class
--------
-- | Describes how to store, and to some extent parse, CSS stylesheets.
-- These methods are used to construct the results from `parse`, etc.
class StyleSheet s where
    -- | Sets the stylesheet priority (useragent vs user vs author), optional.
    setPriority :: Int -> s -> s
    setPriority _ = id
    -- | Stores a parsed selector+properties rule.
    addRule :: s -> StyleRule -> s
    -- | Stores and parses an identified at-rule.
    addAtRule :: s -> Text -> [Token] -> (s, [Token])
    addAtRule self _ tokens = (self, skipAtRule tokens)

-- | Stores the parsed selector*s*+proeprties rule.
addRules :: StyleSheet ss => ss -> ([Selector], ([(Text, [Token])], Text)) -> ss
addRules self (selector:selectors, val@(props, psuedoel)) = addRules self' (selectors, val)
    where self' = addRule self $ StyleRule selector props psuedoel
addRules self ([], _) = self

-- | The properties to set for elements matching the given selector.
data StyleRule = StyleRule Selector [(Text, [Token])] Text deriving (Show, Eq)

-- | Gathers StyleRules into a list, mainly for testing.
data TrivialStyleSheet = TrivialStyleSheet [StyleRule] deriving (Show, Eq)
instance StyleSheet TrivialStyleSheet where
    addRule (TrivialStyleSheet self) rule = TrivialStyleSheet $ rule:self

--------
---- Basic parsing
--------
-- | Parse a CSS stylesheet
parse :: StyleSheet s => s -> Text -> s
parse stylesheet source = parse' stylesheet $ tokenize source

-- | Parse a CSS stylesheet, resolving all URLs to absolute form.
parseForURL :: StyleSheet s => s -> URI -> Text -> s
parseForURL stylesheet base source = parse' stylesheet $ rewriteURLs $ tokenize source
    where
        rewriteURLs (Url text:toks)
            | Just url <- parseRelativeReference $ unpack text =
                Url (pack $ uriToString id (relativeTo url base) "") : rewriteURLs toks
            | otherwise = Function "url" : RightParen : rewriteURLs toks
        rewriteURLs (tok:toks) = tok : rewriteURLs toks
        rewriteURLs [] = []

-- | Parse a tokenized (via `css-syntax`) CSS stylesheet
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
-- | Parse "{key: value; ...}" property values, with a psuedoelement.
parseProperties :: Parser ([(Text, [Token])], Text)
parseProperties (LeftCurlyBracket:tokens) = noPsuedoel $ parseProperties' tokens
parseProperties (Whitespace:tokens) = parseProperties tokens
parseProperties (Colon:Colon:Ident n:tokens) = ((val, n), tokens')
    where ((val, _), tokens') = parseProperties tokens
-- This error recovery is a bit overly conservative, but it's simple.
parseProperties (_:tokens) = noPsuedoel ([], skipAtRule tokens)
parseProperties [] = noPsuedoel ([], [])

noPsuedoel :: (x, y) -> ((x, Text), y)
noPsuedoel (val, tokens) = ((val, ""), tokens)

-- | Parse "key: value;"... property values, as per the HTML "style" property.
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
-- | Returns tokens before & after an at-rule value, terminated after a curly-bracketed block or a semicolon.
scanAtRule :: Parser [Token]
scanAtRule (Semicolon:tokens) = ([Semicolon], tokens)
scanAtRule tokens@(LeftCurlyBracket:_) = scanInner tokens $ \rest -> ([], rest)

scanAtRule tokens@(LeftParen:_) = scanInner tokens scanValue
scanAtRule tokens@(Function _:_) = scanInner tokens scanValue
scanAtRule tokens@(LeftSquareBracket:_) = scanInner tokens scanValue
-- To ensure parens are balanced, should already be handled.
scanAtRule (RightCurlyBracket:tokens) = ([], RightCurlyBracket:tokens)
scanAtRule (RightParen:tokens) = ([], RightParen:tokens)
scanAtRule (RightSquareBracket:tokens) = ([], RightSquareBracket:tokens)

scanAtRule tokens = capture scanAtRule tokens

-- | Returns tokens after an at-rule, as per `scanAtRule`.
skipAtRule :: [Token] -> [Token]
skipAtRule tokens = snd $ scanAtRule tokens

-- | Returns tokens before & after a semicolon.
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

-- | Returns tokens after a semicolon.
skipValue :: [Token] -> [Token]
skipValue tokens = snd $ scanValue tokens
