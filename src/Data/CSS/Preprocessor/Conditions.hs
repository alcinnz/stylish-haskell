{-# LANGUAGE OverloadedStrings #-}
module Data.CSS.Preprocessor.Conditions(
        ConditionalStyles(..), loadImports, expandForMedia
    ) where

import qualified Data.CSS.Preprocessor.Conditions.Expr as Query

import Data.CSS.Syntax.StyleSheet
import Data.CSS.Syntax.Tokens(Token(..))

import Data.Text.Internal (Text(..))
import Data.Text (unpack)
import Network.URI (URI(..), URIAuth(..), parseURI)

import Data.List

data ConditionalStyles s = ConditionalStyles {
    hostURL :: URI,
    mediaDocument :: String,
    inner :: s,
    conditions :: [(Query.Expr, StyleRef)]
}

data StyleRef = External URI | Internal [Token] deriving Eq

hostUrlS :: ConditionalStyles s -> String
hostUrlS = show . hostURL

parseAtBlock :: StyleSheet t => t -> [Token] -> (t, [Token])
parseAtBlock self (LeftCurlyBracket:toks) =
    let (block, toks') = scanBlock toks in (parse' self block, toks')
parseAtBlock self (_:toks) = parseAtBlock self toks
parseAtBlock self [] = (self, [])

instance StyleSheet s => StyleSheet (ConditionalStyles s) where
    setPriority x self = self {inner = setPriority x $ inner self}
    addRule self rule = self {inner = addRule (inner self) rule}

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
            let (block', toks') = scanBlock block in (self {
                conditions = (cond, Internal block') : conditions self
            }, toks')
    addAtRule self "media" tokens = (self, skipAtRule tokens)

    addAtRule self "import" (Whitespace:toks) = addAtRule self "import" toks
    addAtRule self "import" (Url src:toks) = parseAtImport self src toks
    addAtRule self "import" (String src:toks) = parseAtImport self src toks
    addAtRule self "import" tokens = (self, skipAtRule tokens)

    addAtRule self rule tokens =
        let (self', tokens') = addAtRule (inner self) rule tokens in (self {inner = self'}, tokens')

--------
---- @import/@media
--------
parseAtImport :: StyleSheet s => ConditionalStyles s -> Text -> [Token] -> (ConditionalStyles s, [Token])
parseAtImport self src toks
    | (cond, Semicolon:toks') <- Query.parse Semicolon toks, Just uri <- parseURI $ unpack src =
        (self { conditions = (cond, External uri) : conditions self }, toks')
parseAtImport self _ toks = (self, skipAtRule toks)

loadImports :: StyleSheet s => (URI -> IO Text) ->
        (Text -> Query.Datum) -> (Token -> Query.Datum) ->
        ConditionalStyles s -> IO s
loadImports = loadImports' [] []
loadImports' :: StyleSheet s => [URI] -> [(Query.Expr, StyleRef)] -> (URI -> IO Text) ->
        (Text -> Query.Datum) -> (Token -> Query.Datum) ->
        ConditionalStyles s -> IO s
loadImports' blocklist ((cond, src):conds) loader vars evalToken self
    | Query.eval vars evalToken cond, Internal tokens <- src =
        loadImports' blocklist conds loader vars evalToken (parse' self tokens)
    | Query.eval vars evalToken cond, External uri <- src, uri `notElem` blocklist = do
        response <- loader uri
        loadImports' (uri:blocklist) conds loader vars evalToken (parse self response)
    | otherwise = loadImports' blocklist conds loader vars evalToken self
loadImports' blocklist [] loader v t self
    | conds == [] = return $ inner self
    | otherwise = loadImports' blocklist conds loader v t self {conditions = []}
    where conds = conditions self

expandForMedia :: StyleSheet s => (Text -> Query.Datum) -> (Token -> Query.Datum) ->
        ConditionalStyles s -> s
expandForMedia vars evalToken self | conds == [] = inner self
    | otherwise = expandForMedia vars evalToken $
        foldl parse' self {conditions = []} [
            src | (cond, Internal src) <- conds, Query.eval vars evalToken cond
        ]
    where conds = conditions self
