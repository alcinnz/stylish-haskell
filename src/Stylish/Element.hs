module Stylish.Element(
        Element(..), Attribute(..)
    ) where

import Data.Text.Internal (Text(..))

data Element = ElementNode {
    parent :: Maybe Element,
    previous :: Maybe Element,
    name :: Text,
    attributes :: [Attribute] -- in sorted order.
}
data Attribute = Attribute Text String
