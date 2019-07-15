module Data.CSS.Syntax.StylishUtil(
        concatP, capture, skipSpace,
        scanBlock, skipBlock, scanInner,
        Parser
    ) where

import Data.CSS.Syntax.Tokens

type Parser x = [Token] -> (x, [Token])
concatP :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
concatP join left right tokens = (join x y, remainder)
    where
        (x, tokens') = left tokens
        (y, remainder) = right tokens'

capture :: Parser [Token] -> Parser [Token]
capture cb (token:tokens) = (token:captured, tokens')
   where (captured, tokens') = cb tokens
capture _ [] = ([], [])

skipSpace :: [Token] -> [Token]
skipSpace (Whitespace:tokens) = skipSpace tokens
skipSpace tokens = tokens

scanBlock :: Parser [Token]
-- TODO assert closing tags are correct
--    But what should the error recovery be?
scanBlock (RightCurlyBracket:tokens) = ([RightCurlyBracket], tokens)
scanBlock (RightParen:tokens) = ([RightParen], tokens)
scanBlock (RightSquareBracket:tokens) = ([RightSquareBracket], tokens)

scanBlock tokens@(LeftCurlyBracket:_) = scanInner tokens scanBlock
scanBlock tokens@(LeftParen:_) = scanInner tokens scanBlock
scanBlock tokens@(Function _:_) = scanInner tokens scanBlock
scanBlock tokens@(LeftSquareBracket:_) = scanInner tokens scanBlock

scanBlock tokens = capture scanBlock tokens

skipBlock :: [Token] -> [Token]
skipBlock tokens = snd $ scanBlock tokens

scanInner :: [Token] -> Parser [Token] -> ([Token], [Token])
scanInner (token:tokens) cb = concatP gather scanBlock cb tokens
    where gather x y = token : x ++ y
scanInner [] _ = error "Expected a token to capture."
