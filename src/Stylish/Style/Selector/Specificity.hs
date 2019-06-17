module Stylish.Style.Selector.Specificity(
        computeSpecificity
    ) where

import Stylish.Parse.Selector

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