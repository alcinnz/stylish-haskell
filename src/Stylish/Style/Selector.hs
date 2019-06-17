module Stylish.Style.Selector(
        QueryableStyleSheet(..), queryableStyleSheet,
        queryRules
    ) where

import Stylish.Style.Selector.Index
import Stylish.Style.Selector.Interpret
import Stylish.Style.Selector.Specificity
import Stylish.Style.Selector.Importance
import Stylish.Style.Selector.Common

import Stylish.Parse (StyleSheet(..))

ruleStore = ImportanceSplitter $ OrderedRuleStore (InterpretedRuleStore styleIndex) 0

data QueryableStyleSheet store = QueryableStyleSheet {
    store :: store,
    priority :: Int -- author vs user agent vs user styles
}

queryableStyleSheet = QueryableStyleSheet {store = ruleStore, priority = 0}

instance RuleStore s => StyleSheet (QueryableStyleSheet s) where
    addRule self@(QueryableStyleSheet store priority) rule = self {
            store = addStyleRule store priority $ styleRule' rule
        }

queryRules (QueryableStyleSheet store _) el = lookupRules store el
