module Data.CSS.Style.Common(
        RuleStore(..), StyleRule'(..), selector, properties, styleRule',
        Element(..), Attribute(..),
        -- Re-exports
        Text(..), StyleRule(..), Selector(..), SimpleSelector(..), PropertyTest(..)
    ) where

import Data.CSS.Syntax.StyleSheet
import Data.CSS.Syntax.Selector

import Data.Text.Internal (Text(..))

data Element = ElementNode {
    parent :: Maybe Element,
    previous :: Maybe Element,
    name :: Text,
    attributes :: [Attribute] -- in sorted order.
}
data Attribute = Attribute Text String

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
