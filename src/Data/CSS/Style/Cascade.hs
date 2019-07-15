module Data.CSS.Style.Cascade(
        cascade, TrivialPropertyParser(..), PropertyParser(..)
    ) where

import Data.CSS.Style.Common
import Data.CSS.Syntax.Tokens

-- TODO do performance tests to decide beside between strict/lazy,
--      or is another Map implementation better?
import Data.HashMap.Strict
import Data.Text (unpack)

cascadeRules overrides rules = cascadeProperties overrides $ concat $ Prelude.map properties rules
cascadeProperties overrides props = fromList (props ++ overrides)

class PropertyParser a where
    temp :: a
    inherit :: a -> a
    inherit = id

    shorthand :: a -> Text -> [Token] -> [(Text, [Token])]
    shorthand self name value | Just _ <- longhand self self name value = [(name, value)]
        | otherwise = []
    -- longhand parent self name value
    longhand :: a -> a -> Text -> [Token] -> Maybe a

data TrivialPropertyParser = TrivialPropertyParser (HashMap String [Token])
instance PropertyParser TrivialPropertyParser where
    temp = TrivialPropertyParser empty
    longhand _ (TrivialPropertyParser self) key value =
        Just $ TrivialPropertyParser $ insert (unpack key) value self

cascade :: (PropertyParser p, RuleStore s) => s -> Element -> [(Text, [Token])] -> p -> p
cascade self el overrides parent = dispatch parent (inherit parent) $
    toList $ cascadeRules overrides $ lookupRules self el

dispatch parent child ((name, value):props)
    | Just child' <- longhand parent child name value = dispatch parent child' props
    | otherwise = dispatch parent child props
dispatch _ child [] = child
