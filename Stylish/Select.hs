module Stylish.Select(
        DocumentNode(..),
        DocumentAttribute(..)
    ) where

data DocumentNode = DocumentNode {
    parent :: DocumentNode,
    prev :: DocumentNode,
    name :: Text,
    namespace :: Text,
    attributes :: [DocumentAttribute] -- Sorted alphabetically by name.
}

data DocumentAttribute = DocumentAttribute {
    name :: Text,
--    namespace :: Text, -- TODO
    value :: Text
}
