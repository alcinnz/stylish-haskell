module Stylish.Style.Selector.Specificity(
        OrderedRuleStore(..)
    ) where

import Stylish.Parse.Selector
import Stylish.Style.Selector.Common
import Data.List

computeSpecificity :: Selector -> (Int, Int, Int)
computeSpecificity (Element selector) = computeSpecificity' selector
computeSpecificity (Child upSel sel) = computeSpecificity upSel `add` computeSpecificity' sel
computeSpecificity (Descendant upSel sel) = computeSpecificity upSel `add` computeSpecificity' sel
computeSpecificity (Adjacent upSel sel) = computeSpecificity upSel `add` computeSpecificity' sel
computeSpecificity (Sibling upSel sel) = computeSpecificity upSel `add` computeSpecificity' sel

computeSpecificity' (Tag _:sel) = computeSpecificity' sel `add` (0, 0, 1)
computeSpecificity' (Class _:sel) = computeSpecificity' sel `add` (0, 1, 0)
computeSpecificity' (Property _ _:sel) = computeSpecificity' sel `add` (0, 1, 0)
computeSpecificity' (Id _:sel) = computeSpecificity' sel `add` (1, 0, 0)
computeSpecificity' [] = (0, 0, 0)

add :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
add (a, b, c) (x, y, z) = (a + x, b + y, c + z)

---

data OrderedRuleStore inner = OrderedRuleStore inner Int

instance RuleStore inner => RuleStore (OrderedRuleStore inner) where
    addStyleRule (OrderedRuleStore self count) priority rule = OrderedRuleStore (
            addStyleRule self priority $ rule {
                rank = (priority, computeSpecificity $ selector rule, count)
            }
        ) (count + 1)
    lookupRules (OrderedRuleStore self _) el = sort $ lookupRules self el
