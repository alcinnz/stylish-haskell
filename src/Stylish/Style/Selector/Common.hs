module Stylish.Style.Selector.Common(
        RuleStore(..), StyleRule'(..), selector, properties, styleRule'
    ) where

import Stylish.Element
import Stylish.Parse

class RuleStore a where
    new :: a
    addStyleRule :: a -> Int -> StyleRule' -> a
    lookupRules :: a -> Element -> [StyleRule']

type SelectorFunc = Element -> Bool
data StyleRule' = StyleRule' {
    inner :: StyleRule,
    compiledSelector :: SelectorFunc,
    rank :: (Int, (Int, Int, Int), Int) -- This reads ugly, but oh well.
}
styleRule' rule = StyleRule' {
    inner = rule,
    compiledSelector = \_ -> True,
    rank = (0, (0, 0, 0), 0)
}

instance Eq StyleRule' where
    a == b = inner a == inner b
instance Show StyleRule' where show a = show $ inner a
instance Ord StyleRule' where compare x y = rank x `compare` rank y

selector rule | StyleRule selector _ <- inner rule = selector
properties rule | StyleRule _ properties <- inner rule = properties
