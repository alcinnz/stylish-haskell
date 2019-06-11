module Stylish.Parse.Utils(
        concatP, capture, skipSpace
    ) where

import Data.CSS.Syntax.Tokens

concatP join left right tokens = (join x y, remainder)
    where (x, tokens') = left tokens
        (y, remainder) = right tokens'

capture cb token:tokens = (token:captured, tokens')
    where (captured, tokens') = cb tokens

skipSpace Whitespace:tokens = skipSpace tokens
skipSpace tokens = tokens
