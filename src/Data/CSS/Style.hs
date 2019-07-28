{-# LANGUAGE OverloadedStrings #-}
module Data.CSS.Style(
        QueryableStyleSheet, QueryableStyleSheet'(..), queryableStyleSheet,
        queryRules,
        PropertyParser(..), cascade, cascade',
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

import Data.CSS.Syntax.Tokens (Token)
import Data.CSS.Syntax.StyleSheet (StyleSheet(..))
import Data.HashMap.Strict (HashMap, lookupDefault)

type QueryableStyleSheet parser = QueryableStyleSheet' (ImportanceSplitter (
        PropertyExpander parser (OrderedRuleStore (InterpretedRuleStore StyleIndex))
    )) parser

data QueryableStyleSheet' store parser = QueryableStyleSheet' {
    store :: store,
    parser :: parser,
    priority :: Int -- author vs user agent vs user styles
}

queryableStyleSheet :: PropertyParser p => QueryableStyleSheet p
queryableStyleSheet = QueryableStyleSheet' {store = new, parser = temp, priority = 0}

instance (RuleStore s, PropertyParser p) => StyleSheet (QueryableStyleSheet' s p) where
    setPriority v self = self {priority = v}
    addRule self@(QueryableStyleSheet' store' _ priority') rule = self {
            store = addStyleRule store' priority' $ styleRule' rule
        }

--- Reexpose cascade methods
queryRules :: (PropertyParser p, RuleStore s) =>
    QueryableStyleSheet' s p -> Element -> HashMap Text [StyleRule']
queryRules (QueryableStyleSheet' store' _ _) = Cascade.query store'

cascade' :: PropertyParser p => [StyleRule'] -> Props -> p -> p
cascade' = Cascade.cascade

--- Facade for trivial cases
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
