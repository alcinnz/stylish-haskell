-- | Utility parser combinators for parsing CSS stylesheets.
module Data.CSS.Syntax.StylishUtil(
        concatP, capture, skipSpace,
        scanBlock, skipBlock, scanInner,
        Parser
    ) where

import Data.CSS.Syntax.Tokens

-- | A simple parser combinator type.
type Parser x = [Token] -> (x, [Token])

-- | Chains two parser combinators together.
concatP :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
concatP join left right tokens = (join x y, remainder)
    where
        (x, tokens') = left tokens
        (y, remainder) = right tokens'

-- | "captures" the token being parsed into the returned output.
capture :: Parser [Token] -> Parser [Token]
capture cb (token:tokens) = (token:captured, tokens')
   where (captured, tokens') = cb tokens
capture _ [] = ([], [])

-- | Removes preceding `Whitespace` tokens.
skipSpace :: [Token] -> [Token]
skipSpace (Whitespace:tokens) = skipSpace tokens
skipSpace tokens = tokens

-- | Returns tokens until the next unbalanced closing brace.
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

-- | Returns tokens after the next unbalanced closing brace.
skipBlock :: [Token] -> [Token]
skipBlock tokens = snd $ scanBlock tokens

-- | Parses a block followed by the given combinator, returning the tokens the matched.
scanInner :: [Token] -> Parser [Token] -> ([Token], [Token])
scanInner (token:tokens) cb = concatP gather scanBlock cb tokens
    where gather x y = token : x ++ y
scanInner [] _ = error "Expected a token to capture."
