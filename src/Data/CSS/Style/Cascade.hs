{-# LANGUAGE OverloadedStrings #-}
-- | Applies CSS selection, cascade, & inheritance.
-- INTERNAL MODULE.
module Data.CSS.Style.Cascade(
        query, cascade,
        TrivialPropertyParser(..), PropertyParser(..), Props
    ) where

import Data.CSS.Style.Common
import Data.CSS.Syntax.Tokens

-- TODO do performance tests to decide beside between strict/lazy,
--      or is another Map implementation better?
import Data.HashMap.Strict
import Data.Text (unpack, pack, isPrefixOf)

-- | Defines how to parse CSS properties into an output "style" format.
class PropertyParser a where
    -- | Default styles.
    temp :: a
    -- | Creates a style inherited from a parent style.
    inherit :: a -> a
    inherit = id

    -- | Expand a shorthand property into longhand properties.
    shorthand :: a -> Text -> [Token] -> [(Text, [Token])]
    shorthand self key value | Just _ <- longhand self self key value = [(key, value)]
        | otherwise = []
    -- longhand parent self name value
    longhand :: a -> a -> Text -> [Token] -> Maybe a

    -- | Retrieve stored variables, optional.
    getVars :: a -> Props
    getVars _ = []
    -- | Save variable values, optional.
    setVars :: Props -> a -> a
    setVars _ = id

-- | Gather properties into a hashmap.
data TrivialPropertyParser = TrivialPropertyParser (HashMap String [Token])
instance PropertyParser TrivialPropertyParser where
    temp = TrivialPropertyParser empty
    longhand _ (TrivialPropertyParser self) key value =
        Just $ TrivialPropertyParser $ insert (unpack key) value self

-- | "key: value;" entries to be parsed into an output type.
type Props = [(Text, [Token])]

--------
---- Query/Psuedo-elements
--------

-- | Looks up style rules for an element, grouped by psuedoelement.
query :: RuleStore s => s -> Element -> HashMap Text [StyleRule']
query self el = Prelude.foldr yield empty $ lookupRules self el
    where yield rule store = insertWith (++) (psuedoElement rule) [resolveAttr rule el] store

--------
---- Cascade/Inheritance
--------

-- | Applies cascade for the given `StyleRule'`s & explicit styles,
-- parsed to a value of the same `PropertyParser` type passed in & inheriting from it.
cascade :: PropertyParser p => [StyleRule'] -> Props -> p -> p
cascade styles overrides base =
    construct base $ toList $ cascadeRules (getVars base ++ overrides) styles

cascadeRules :: Props -> [StyleRule'] -> HashMap Text [Token]
cascadeRules overrides rules = cascadeProperties overrides $ concat $ Prelude.map properties rules
cascadeProperties :: Props -> Props -> HashMap Text [Token]
cascadeProperties overrides props = fromList (props ++ overrides)

construct :: PropertyParser p => p -> Props -> p
construct base props = dispatch base child props
    where child = setVars [item | item@(n, _) <- props, isPrefixOf "--" n] $ inherit base
dispatch :: PropertyParser p => p -> p -> Props -> p
dispatch base child ((key, value):props)
    | Just child' <- longhand base child key value = dispatch base child' props
    | otherwise = dispatch base child props
dispatch _ child [] = child

--------
---- attr()
--------
resolveAttr :: StyleRule' -> Element -> StyleRule'
resolveAttr self el = self {
        inner = StyleRule sel [(n, resolveAttr' v $ attrs2Dict el) | (n, v) <- attrs] psuedo
    } where StyleRule sel attrs psuedo = inner self

attrs2Dict :: Element -> HashMap Text String
attrs2Dict el = fromList [(a, b) | Attribute a b <- attributes el]

resolveAttr' :: [Token] -> HashMap Text String  -> [Token]
resolveAttr' (Function "attr":Ident attr:RightParen:toks) attrs =
    String (pack $ lookupDefault "" attr attrs) : resolveAttr' toks attrs
resolveAttr' (tok:toks) attrs = tok : resolveAttr' toks attrs
resolveAttr' [] _ = []
