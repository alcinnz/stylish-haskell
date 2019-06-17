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

-- TODO do performance tests to decide beside between strict/lazy,
--      or is another Map implementation better?
import Data.HashMap.Strict
import Data.Text.Internal (Text(..))
import Data.CSS.Syntax.Tokens

type QueryableStyleSheet = QueryableStyleSheet' (ImportanceSplitter (
        OrderedRuleStore (InterpretedRuleStore StyleIndex)
    ))

data QueryableStyleSheet' store = QueryableStyleSheet' {
    store :: store,
    priority :: Int -- author vs user agent vs user styles
}

queryableStyleSheet :: QueryableStyleSheet
queryableStyleSheet = QueryableStyleSheet' {store = new, priority = 0}

instance RuleStore s => StyleSheet (QueryableStyleSheet' s) where
    addRule self@(QueryableStyleSheet' store priority) rule = self {
            store = addStyleRule store priority $ styleRule' rule
        }

queryRules (QueryableStyleSheet' store _) el = lookupRules store el

--------
---- Cascade
--------

cascadeRules rules = cascadeProperties $ concat $ Prelude.map properties rules

cascadeProperties ((name, value):props) = insert name value $ cascadeProperties props
