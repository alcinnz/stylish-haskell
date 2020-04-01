{-# LANGUAGE OverloadedStrings #-}
-- | Queries computed styles out of a specially-parsed CSS stylesheet.
-- See in particular `QueryableStyleSheet`, `queryRules`, & `cascade'`.
module Data.CSS.Style(
        QueryableStyleSheet, QueryableStyleSheet'(..), queryableStyleSheet,
        queryRules,
        PropertyParser(..), cascade, cascade', VarParser(..),
        TrivialPropertyParser(..),
        Element(..), Attribute(..)
    ) where

import Data.CSS.Style.Selector.Index
import Data.CSS.Style.Selector.Interpret
import Data.CSS.Style.Selector.Specificity
import Data.CSS.Style.Importance
import Data.CSS.Style.Common
import qualified Data.CSS.Style.Cascade as Cascade
import Data.CSS.Style.Cascade (PropertyParser(..), TrivialPropertyParser, Props)

import Data.CSS.Syntax.Tokens (Token(..))
import Data.CSS.Syntax.StyleSheet (StyleSheet(..))
import Data.HashMap.Strict (HashMap, lookupDefault, fromList)
import Data.Text (isPrefixOf)
import Data.List (elemIndex)

-- | A parsed CSS stylesheet from which you can query styles to match an element.
type QueryableStyleSheet parser = QueryableStyleSheet' (ImportanceSplitter (
        PropertyExpander parser (OrderedRuleStore (InterpretedRuleStore StyleIndex))
    )) parser

-- | More generic version of `QueryableStyleSheet`.
data QueryableStyleSheet' store parser = QueryableStyleSheet' {
    -- | Internal datastructure for efficient style lookup.
    store :: store,
    -- | The "PropertyParser" to use for property syntax validation.
    parser :: parser,
    -- | Whether author, useragent, or user styles are currently being parsed.
    priority :: Int -- author vs user agent vs user styles
}

-- | Constructs an empty QueryableStyleSheet'.
queryableStyleSheet :: PropertyParser p => QueryableStyleSheet p
queryableStyleSheet = QueryableStyleSheet' {store = new, parser = temp, priority = 0}

instance (RuleStore s, PropertyParser p) => StyleSheet (QueryableStyleSheet' s p) where
    setPriority v self = self {priority = v}
    addRule self@(QueryableStyleSheet' store' _ priority') rule = self {
            store = addStyleRule store' priority' $ styleRule' rule
        }

--- Reexpose cascade methods
-- | Looks up style rules matching the specified element, grouped by psuedoelement.
queryRules :: (PropertyParser p, RuleStore s) =>
    QueryableStyleSheet' s p -> Element -> HashMap Text [StyleRule']
queryRules (QueryableStyleSheet' store' _ _) = Cascade.query store'

-- | Selects used property values from the given style rules,
-- & populates into a new `PropertyParser` inheriting from the one given.
cascade' :: PropertyParser p => [StyleRule'] -> Props -> p -> p
cascade' = Cascade.cascade

-- | Facade over `queryRules` & `cascade'` for simple cases you don't care about psuedoelements.
cascade :: PropertyParser p => QueryableStyleSheet p -> Element -> Props -> p -> p
cascade self el = cascade' $ lookupDefault [] "" $ queryRules self el

--- Verify syntax during parsing, so invalid properties don't interfere with cascade.
data PropertyExpander parser inner = PropertyExpander parser inner
instance (PropertyParser parser, RuleStore inner) => RuleStore (PropertyExpander parser inner) where
    new = PropertyExpander temp new
    addStyleRule (PropertyExpander parser' inner') priority' rule =
        PropertyExpander parser' $ addStyleRule inner' priority' $ expandRule parser' rule
    lookupRules (PropertyExpander _ inner') el = lookupRules inner' el

expandRule :: PropertyParser t => t -> StyleRule' -> StyleRule'
expandRule parser' rule = rule {inner = StyleRule sel (expandProperties parser' props) psuedo}
    where (StyleRule sel props psuedo) = inner rule
expandProperties :: PropertyParser t => t -> [(Text, [Token])] -> [(Text, [Token])]
expandProperties parser' ((key, value):props) =
        shorthand parser' key value ++ expandProperties parser' props
expandProperties _ [] = []

--------
---- var()
--------
-- | `PropertyParser` that lowers var() calls before forwarding to another.
data VarParser a = VarParser {vars :: Props, innerParser :: a}

instance PropertyParser p => PropertyParser (VarParser p) where
    temp = VarParser [] temp
    inherit (VarParser vars' self) = VarParser vars' $ inherit self

    shorthand self name' value
        | Function "var" `elem` value || "--" `isPrefixOf` name' = [(name', value)] -- Fail during inheritance...
        | otherwise = shorthand (innerParser self) name' value
    longhand parent' self@(VarParser vars' inner') name' value
        | Function "var" `elem` value = resolveVars value (fromList vars') >>= longhand parent' self name'
        | otherwise = VarParser vars' <$> longhand (innerParser parent') inner' name' value

    getVars = vars
    setVars v self = self {vars = v}

resolveVars :: [Token] -> HashMap Text [Token] -> Maybe [Token]
resolveVars (Function "var":Ident var:RightParen:toks) ctxt = (lookupDefault [] var ctxt ++) <$> resolveVars toks ctxt
resolveVars (Function "var":Ident var:Comma:toks) ctxt
    | Just i <- RightParen `elemIndex` toks, (fallback, RightParen:toks') <- i `splitAt` toks =
        (lookupDefault fallback var ctxt ++) <$> resolveVars toks' ctxt
resolveVars (Function "var":_) _ = Nothing
resolveVars (tok:toks) ctxt = (tok:) <$> resolveVars toks ctxt
resolveVars [] _ = Just []
