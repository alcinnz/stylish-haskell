{-# LANGUAGE OverloadedStrings #-}
module Data.CSS.Preprocessor.Conditions(
        ConditionalStyles(..), conditionalStyles,
        extractImports, resolveImports, resolve
    ) where

import qualified Data.CSS.Preprocessor.Conditions.Expr as Query

import Data.CSS.Syntax.StyleSheet
import Data.CSS.Syntax.Selector
import Data.CSS.Syntax.Tokens(Token(..))
import Data.CSS.Style (PropertyParser(..))

import Data.Text.Internal (Text(..))
import Data.Text (unpack)
import Network.URI (URI(..), URIAuth(..), parseURI)

import Data.List

data ConditionalStyles p = ConditionalStyles {
    hostURL :: URI,
    mediaDocument :: String,
    rules :: [ConditionalRule p],
    propertyParser :: p
}

conditionalStyles :: PropertyParser p => URI -> String -> ConditionalStyles p
conditionalStyles uri mediaDocument = ConditionalStyles uri mediaDocument [] temp

data ConditionalRule p = Priority Int | StyleRule' StyleRule | AtRule Text [Token] |
    External Query.Expr URI | Internal Query.Expr (ConditionalStyles p)

addRule' :: ConditionalStyles p -> ConditionalRule p -> ConditionalStyles p
addRule' self rule = self {rules = rule : rules self}

hostUrlS :: ConditionalStyles p -> String
hostUrlS = show . hostURL

parseAtBlock :: StyleSheet t => t -> [Token] -> (t, [Token])
parseAtBlock self (LeftCurlyBracket:toks) =
    let (block, toks') = scanBlock toks in (parse' self block, toks')
parseAtBlock self (_:toks) = parseAtBlock self toks
parseAtBlock self [] = (self, [])

instance PropertyParser p => StyleSheet (ConditionalStyles p) where
    setPriority x self = addRule' self $ Priority x
    addRule self rule = addRule' self $ StyleRule' rule

    addAtRule self "document" (Whitespace:toks) = addAtRule self "document" toks
    addAtRule self "document" (Comma:toks) = addAtRule self "document" toks
    addAtRule self "document" (Url match:toks)
        | unpack match == hostUrlS self = parseAtBlock self toks
        | otherwise = addAtRule self "document" toks
    addAtRule self "document" (Function "url-prefix":String match:RightParen:toks)
        | unpack match `isPrefixOf` hostUrlS self = parseAtBlock self toks
        | otherwise = addAtRule self "document" toks
    addAtRule self "document" (Function "domain":String match:RightParen:toks)
        | unpack match == domain || ('.':unpack match) `isSuffixOf` domain =
            parseAtBlock self toks
        | otherwise = addAtRule self "document" toks
        where
            domain | Just auth <- uriAuthority $ hostURL self = uriRegName auth
                | otherwise = ""
    addAtRule self "document" (Function "media-document":String match:RightParen:toks)
        | unpack match == mediaDocument self = parseAtBlock self toks
        | otherwise = addAtRule self "document" toks
    -- TODO Support regexp() conditions, requires new dependency
    addAtRule self "document" tokens = (self, skipAtRule tokens)

    addAtRule self "media" toks
        | (cond, LeftCurlyBracket:block) <- Query.parse LeftCurlyBracket toks =
            let (block', toks') = scanBlock block in
                (addRule' self $ Internal cond $ parse' self {rules = []} block', toks')
    addAtRule self "media" tokens = (self, skipAtRule tokens)

    addAtRule self "import" (Whitespace:toks) = addAtRule self "import" toks
    addAtRule self "import" (Url src:toks) = parseAtImport self src toks
    addAtRule self "import" (String src:toks) = parseAtImport self src toks
    addAtRule self "import" tokens = (self, skipAtRule tokens)

    addAtRule self "supports" toks =
            let (cond, toks') = break (== LeftCurlyBracket) toks in
            if evalSupports (propertyParser self) cond
                then parseAtBlock self toks' else (self, skipAtRule toks')

    addAtRule self rule tokens = let (block, rest) = scanAtRule tokens in
        (addRule' self $ AtRule rule block, rest)
--------
---- @import/@media
--------
parseAtImport :: PropertyParser p => ConditionalStyles p -> Text ->
        [Token] -> (ConditionalStyles p, [Token])
parseAtImport self src toks
    | (cond, Semicolon:toks') <- Query.parse Semicolon toks, Just uri <- parseURI $ unpack src =
        (addRule' self $ External cond uri, toks')
parseAtImport self _ toks = (self, skipAtRule toks)

extractImports :: (Text -> Query.Datum) -> (Token -> Query.Datum) -> ConditionalStyles p -> [URI]
extractImports vars evalToken self =
    [uri | External cond uri <- rules self, Query.eval vars evalToken cond]

resolveImports :: ConditionalStyles p -> [(URI, ConditionalStyles p)] -> ConditionalStyles p
resolveImports self responses = self {rules = map resolveImport $ rules self}
    where
        resolveImport (External cond uri) | (body:_) <- [body | (uri', body) <- responses, uri' == uri] =
            Internal cond body
        resolveImport x = x

resolve :: StyleSheet s => (Text -> Query.Datum) -> (Token -> Query.Datum) ->
        s -> ConditionalStyles p -> s
resolve v t styles self = resolve' v t (reverse $ rules self) styles
resolve' :: StyleSheet s => (Text -> Query.Datum) -> (Token -> Query.Datum) ->
        [ConditionalRule p] -> s -> s
resolve' v t (Priority x:rules) styles = resolve' v t rules $ setPriority x styles
resolve' v t (StyleRule' rule:rules) styles = resolve' v t rules $ addRule styles rule
resolve' v t (AtRule name block:rules) styles = resolve' v t rules $ fst $ addAtRule styles name block
resolve' v t (Internal cond block:rules) styles | Query.eval v t cond =
    resolve' v t rules $ resolve v t styles block
resolve' v t (_:rules) styles = resolve' v t rules styles
resolve' _ _ [] styles = styles

--------
---- @supports
--------

evalSupports :: PropertyParser p => p -> [Token] -> Bool
evalSupports self (Whitespace:toks) = evalSupports self toks
evalSupports self (Ident "not":toks) = not $ evalSupports self toks
evalSupports self (LeftParen:toks) = let (block, toks') = scanBlock toks in
    evalSupportsOp toks' self $ supportsProperty block self
evalSupports self (Function "selector":toks) = let (block, toks') = scanBlock toks in
    evalSupportsOp toks' self $ supportsSelector block
evalSupports _ _ = False

evalSupportsOp :: PropertyParser p => [Token] -> p -> Bool -> Bool
evalSupportsOp (Whitespace:toks) self right = evalSupportsOp toks self right
evalSupportsOp (Ident "and":toks) self right = right && evalSupports self toks
evalSupportsOp (Ident "or":toks) self right = right || evalSupports self toks
evalSupportsOp [RightParen] _ ret = ret -- scanBlock captures closing paren
evalSupportsOp [] _ ret = ret
evalSupportsOp _ _ _ = False

supportsProperty :: PropertyParser p => [Token] -> p -> Bool
supportsProperty (Whitespace:toks) self = supportsProperty toks self
supportsProperty toks@(Ident "not":_) self = evalSupports self toks -- Special case fallback
supportsProperty (Ident key:toks) self
    | (Colon:value) <- skipSpace toks = -- "init"'s used to strip trailing RightParen
        shorthand self key (filter (/= Whitespace) $ init value) /= []
    | skipSpace toks `elem` [[RightParen], []] = shorthand self key [Ident "initial"] /= []
    | otherwise = False
supportsProperty toks self = evalSupports self toks -- Fallback to parenthesized expression.

supportsSelector :: [Token] -> Bool
supportsSelector toks = let (sels, toks') = parseSelectors toks in
    sels /= [] && (toks' == [] || toks' == [RightParen])
