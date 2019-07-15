module Data.CSS.Style(
        QueryableStyleSheet, QueryableStyleSheet'(..), queryableStyleSheet,
        queryRules,
        PropertyParser(..), cascade,
        TrivialPropertyParser(..),
        Element(..), Attribute(..)
    ) where

import Data.CSS.Style.Selector.Index
import Data.CSS.Style.Selector.Interpret
import Data.CSS.Style.Selector.Specificity
import Data.CSS.Style.Importance
import Data.CSS.Style.Common
import qualified Data.CSS.Style.Cascade as Cascade
import Data.CSS.Style.Cascade (PropertyParser(..), TrivialPropertyParser)

import Data.CSS.Syntax.Tokens (Token)
import Data.CSS.Syntax.StyleSheet (StyleSheet(..))

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
    addRule self@(QueryableStyleSheet' store' _ priority') rule = self {
            store = addStyleRule store' priority' $ styleRule' rule
        }

queryRules :: PropertyParser p => QueryableStyleSheet p -> Element -> [StyleRule']
queryRules (QueryableStyleSheet' store' _ _) el = lookupRules store' el

--------
---- Cascade
--------

cascade :: PropertyParser p => QueryableStyleSheet p -> Element -> [(Text, [Token])] -> p -> p
cascade (QueryableStyleSheet' store' _ _) = Cascade.cascade store'

--- Verify syntax during parsing, so invalid properties don't interfere with cascade.
data PropertyExpander parser inner = PropertyExpander parser inner
instance (PropertyParser parser, RuleStore inner) => RuleStore (PropertyExpander parser inner) where
    new = PropertyExpander temp new
    addStyleRule (PropertyExpander parser' inner') priority' rule =
        PropertyExpander parser' $ addStyleRule inner' priority' $ expandRule parser' rule
    lookupRules (PropertyExpander _ inner') el = lookupRules inner' el

expandRule :: PropertyParser t => t -> StyleRule' -> StyleRule'
expandRule parser' rule = rule {inner = StyleRule sel $ expandProperties parser' props}
    where (StyleRule sel props) = inner rule
expandProperties :: PropertyParser t => t -> [(Text, [Token])] -> [(Text, [Token])]
expandProperties parser' ((key, value):props) =
        shorthand parser' key value ++ expandProperties parser' props
expandProperties _ [] = []
