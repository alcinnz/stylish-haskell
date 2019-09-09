{-# LANGUAGE OverloadedStrings #-}
module Data.CSS.Preprocessor.Conditions(
        ConditionalStyles(..)
    ) where

import Data.CSS.Syntax.StyleSheet
import Data.CSS.Syntax.Tokens(Token(..))

import Data.Text (unpack)
import Network.URI (URI(..), URIAuth(..))

import Data.List

data ConditionalStyles s = ConditionalStyles {
    hostURL :: URI,
    mediaDocument :: String,
    inner :: s
}

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

    addAtRule self rule tokens =
        let (self', tokens') = addAtRule (inner self) rule tokens in (self {inner = self'}, tokens')
