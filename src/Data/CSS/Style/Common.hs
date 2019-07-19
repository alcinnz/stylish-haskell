module Data.CSS.Style.Common(
        RuleStore(..), StyleRule'(..), selector, properties, psuedoElement, styleRule',
        Element(..), Attribute(..),
        -- Re-exports
        Text(..), StyleRule(..), Selector(..), SimpleSelector(..), PropertyTest(..)
    ) where

import Data.CSS.Syntax.StyleSheet
import Data.CSS.Syntax.Selector
import Data.CSS.Syntax.Tokens
import Data.Text.Internal (Text(..))

data Element = ElementNode {
    parent :: Maybe Element,
    previous :: Maybe Element,
    name :: Text,
    attributes :: [Attribute] -- in sorted order.
}
data Attribute = Attribute Text String deriving (Eq, Ord)

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
styleRule' :: StyleRule -> StyleRule'
styleRule' rule = StyleRule' {
    inner = rule,
    compiledSelector = \_ -> True,
    rank = (0, (0, 0, 0), 0)
}

instance Eq StyleRule' where
    a == b = inner a == inner b
instance Show StyleRule' where show a = show $ inner a
instance Ord StyleRule' where compare x y = rank x `compare` rank y

selector :: StyleRule' -> Selector
selector rule | StyleRule sel _ _ <- inner rule = sel
properties :: StyleRule' -> [(Text, [Data.CSS.Syntax.Tokens.Token])]
properties rule | StyleRule _ props _ <- inner rule = props
psuedoElement :: StyleRule' -> Text
psuedoElement rule | StyleRule _ _ psuedo <- inner rule = psuedo
