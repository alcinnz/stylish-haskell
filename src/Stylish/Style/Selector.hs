module Stylish.Style.Selector(
        
    ) where

import Stylish.Style.Selector.Index
import Stylish.Style.Selector.Interpret
import Stylish.Style.Selector.Specificity
import Stylish.Style.Selector.Importance
import Stylish.Style.Selector.Common

ruleStore = ImportanceSplitter $ OrderedRuleStore (InterpretedRuleStore styleIndex) 0

data QueryableStyleSheet store = QueryableStyleSheet {
    store :: store,
    priority :: Int -- author vs user agent vs user styles
}

queryableStyleSheet = QueryableStyleSheet {store = ruleStore, priority = 0}
