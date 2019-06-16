module Stylish.Element(
        Element(..), Attribute(..)
    ) where

data Element = ElementNode {
    parent :: Maybe Element,
    previous :: Maybe Element,
    name :: String,
    attributes :: [Attribute] -- in sorted order.
}
data Attribute = Attribute String String
