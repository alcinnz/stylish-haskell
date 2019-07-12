module Data.CSS.Style(
        QueryableStyleSheet(..), queryableStyleSheet,
        queryRules,
        PropertyParser(..), cascade
    ) where

import Data.CSS.Style.Selector.Index
import Data.CSS.Style.Selector.Interpret
import Data.CSS.Style.Selector.Specificity
import Data.CSS.Style.Importance
import Data.CSS.Style.Common
import Data.CSS.Syntax.StyleSheet (StyleSheet(..))

-- TODO do performance tests to decide beside between strict/lazy,
--      or is another Map implementation better?
import Data.HashMap.Strict
import Data.CSS.Syntax.Tokens

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
    addRule self@(QueryableStyleSheet' store _ priority) rule = self {
            store = addStyleRule store priority $ styleRule' rule
        }

queryRules (QueryableStyleSheet' store _ _) el = lookupRules store el

--------
---- Cascade
--------

cascadeRules rules = cascadeProperties $ concat $ Prelude.map properties rules

cascadeProperties ((name, value):props) = insert name value $ cascadeProperties props

--------
---- Dispatch to property definitions
--------

class PropertyParser a where
    temp :: a
    shorthand :: a -> Text -> [Token] -> [(Text, [Token])]
    shorthand self name value | Just _ <- longhand self self name value = [(name, value)]
        | otherwise = []
    -- longhand parent self name value
    longhand :: a -> a -> Text -> [Token] -> Maybe a

cascade :: PropertyParser p => QueryableStyleSheet p -> Element -> p -> p
cascade self el parent = dispatch parent parent $ toList $ cascadeRules $ queryRules self el

dispatch parent child ((name, value):props)
    | Just child' <- longhand parent child name value = dispatch parent child' props
    | otherwise = dispatch parent child props

--- Verify syntax during parsing, so invalid properties don't interfere with cascade.
data PropertyExpander parser inner = PropertyExpander parser inner
instance (PropertyParser parser, RuleStore inner) => RuleStore (PropertyExpander parser inner) where
    new = PropertyExpander temp new
    addStyleRule (PropertyExpander parser inner) priority rule =
        PropertyExpander parser $ addStyleRule inner priority $ expandRule parser rule
    lookupRules (PropertyExpander _ inner) el = lookupRules inner el

expandRule parser rule = rule {inner = StyleRule selector $ expandProperties parser properties}
    where (StyleRule selector properties) = inner rule
expandProperties parser ((name, value):props) =
        shorthand parser name value ++ expandProperties parser props
expandProperties _ [] = []
