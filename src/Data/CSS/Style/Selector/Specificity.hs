{-# LANGUAGE OverloadedStrings #-}
module Data.CSS.Style.Selector.Specificity(
        OrderedRuleStore(..)
    ) where

import Data.CSS.Syntax.Selector
import Data.CSS.Style.Common
import Data.List

type Vec = (Int, Int, Int)
computeSpecificity :: Text -> Selector -> Vec
computeSpecificity "" (Element sel) = computeSpecificity' sel
computeSpecificity "" (Child upSel sel) = computeSpecificity "" upSel `add` computeSpecificity' sel
computeSpecificity "" (Descendant upSel sel) = computeSpecificity "" upSel `add` computeSpecificity' sel
computeSpecificity "" (Adjacent upSel sel) = computeSpecificity "" upSel `add` computeSpecificity' sel
computeSpecificity "" (Sibling upSel sel) = computeSpecificity "" upSel `add` computeSpecificity' sel
computeSpecificity _ _ = (0, 0, 1) -- psuedoelements count as a tag.

computeSpecificity' :: [SimpleSelector] -> Vec
computeSpecificity' (Tag _:sel) = computeSpecificity' sel `add` (0, 0, 1)
computeSpecificity' (Class _:sel) = computeSpecificity' sel `add` (0, 1, 0)
computeSpecificity' (Psuedoclass _ _:sel) = computeSpecificity' sel `add` (0, 1, 0)
computeSpecificity' (Property _ _:sel) = computeSpecificity' sel `add` (0, 1, 0)
computeSpecificity' (Id _:sel) = computeSpecificity' sel `add` (1, 0, 0)
computeSpecificity' [] = (0, 0, 0)

add :: Vec -> Vec -> Vec
add (a, b, c) (x, y, z) = (a + x, b + y, c + z)

data OrderedRuleStore inner = OrderedRuleStore inner Int

instance RuleStore inner => RuleStore (OrderedRuleStore inner) where
    new = OrderedRuleStore new 0
    addStyleRule (OrderedRuleStore self count) priority rule = OrderedRuleStore (
            addStyleRule self priority $ rule {
                rank = (priority, computeSpecificity (psuedoElement rule) $ selector rule, count)
            }
        ) (count + 1)
    lookupRules (OrderedRuleStore self _) el = sort $ lookupRules self el
