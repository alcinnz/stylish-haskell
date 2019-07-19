module Data.CSS.Style.Cascade(
        query, cascade,
        TrivialPropertyParser(..), PropertyParser(..), Props
    ) where

import Data.CSS.Style.Common
import Data.CSS.Syntax.Tokens

-- TODO do performance tests to decide beside between strict/lazy,
--      or is another Map implementation better?
import Data.HashMap.Strict
import Data.Text (unpack)

class PropertyParser a where
    temp :: a
    inherit :: a -> a
    inherit = id

    shorthand :: a -> Text -> [Token] -> [(Text, [Token])]
    shorthand self key value | Just _ <- longhand self self key value = [(key, value)]
        | otherwise = []
    -- longhand parent self name value
    longhand :: a -> a -> Text -> [Token] -> Maybe a

data TrivialPropertyParser = TrivialPropertyParser (HashMap String [Token])
instance PropertyParser TrivialPropertyParser where
    temp = TrivialPropertyParser empty
    longhand _ (TrivialPropertyParser self) key value =
        Just $ TrivialPropertyParser $ insert (unpack key) value self

type Props = [(Text, [Token])]

--- The query step exposes the available psuedoelements to the caller.

query :: RuleStore s => s -> Element -> HashMap Text [StyleRule']
query self el = Prelude.foldr yield empty $ lookupRules self el
    where yield rule store = insertWith (++) (psuedoElement rule) [rule] store

cascade :: PropertyParser p => [StyleRule'] -> Props -> p -> p
cascade styles overrides base =
    dispatch base (inherit base) $ toList $ cascadeRules overrides styles

cascadeRules :: Props -> [StyleRule'] -> HashMap Text [Token]
cascadeRules overrides rules = cascadeProperties overrides $ concat $ Prelude.map properties rules
cascadeProperties :: Props -> Props -> HashMap Text [Token]
cascadeProperties overrides props = fromList (props ++ overrides)

dispatch :: PropertyParser p => p -> p -> Props -> p
dispatch base child ((key, value):props)
    | Just child' <- longhand base child key value = dispatch base child' props
    | otherwise = dispatch base child props
dispatch _ child [] = child
