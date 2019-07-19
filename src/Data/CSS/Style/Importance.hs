{-# LANGUAGE OverloadedStrings #-}
module Data.CSS.Style.Importance (
        ImportanceSplitter(..)
    ) where

import Data.CSS.Syntax.Tokens
import Data.CSS.Style.Common

type Property = (Text, [Token])
splitProperties :: [Property] -> ([Property], [Property])
splitProperties (prop@(key, value):rest)
        | (Ident "important":Delim '!':value') <- reverse value =
            (unimportant, (key, reverse value'):important)
        | otherwise = (prop:unimportant, important)
    where (unimportant, important) = splitProperties rest
splitProperties [] = ([], [])

--- NOTE: Prorities are defined with lower numbers being more important,
---     so negate to be consistant with other priority sources.
--- This API decision started out being accidental, but I find it more intuitive.
data ImportanceSplitter a = ImportanceSplitter a
instance RuleStore inner => RuleStore (ImportanceSplitter inner) where
    new = ImportanceSplitter new
    addStyleRule (ImportanceSplitter self) priority rule =
            ImportanceSplitter $ addStyleRule (
                addStyleRule self (negate priority) $ buildRule unimportant
            ) priority $ buildRule important
        where
            (unimportant, important) = splitProperties props
            (StyleRule sel props psuedo) = inner rule
            buildRule x = rule {inner = StyleRule sel x psuedo}
    lookupRules (ImportanceSplitter self) el = lookupRules self el
