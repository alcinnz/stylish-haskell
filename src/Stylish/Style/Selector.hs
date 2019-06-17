module Stylish.Style.Selector(
        
    ) where

import Stylish.Style.Selector.Index
import Stylish.Style.Selector.Interpret
import Stylish.Style.Selector.Specificity
import Stylish.Style.Selector.Common

ruleStore = OrderedRuleStore (InterpretedRuleStore styleIndex) 0

data QueryableStyleSheet = QueryableStyleSheet {
    store :: RuleStore,
    priority :: Int -- author vs user agent vs user styles
}

queryableStyleSheet = QueryableStyleSheet {store = ruleStore, proirity = 0}
