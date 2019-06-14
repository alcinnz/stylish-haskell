module Stylish.Style(
        style,
        Style(..),
        Element(..),
        Attribute(..),
    ) where

style :: Style a => Stylist a -> Element -> a
-- Stylist consists of internal types, implements StyleSheet.

class Style s where
    initial :: s
    shorthand :: Text -> [Token] -> [(Text, [Token])]
    shorthand name value
        | Just _ <- longhand initial initial name value = (name, value)
        | otherwise = []
    longhand :: s -> s -> Text -> [Token] -> Maybe s

data Element = Element {
    prev :: Maybe Element,
    parent :: Maybe Element,
    name :: Text,
    attrs :: [Attribute]
}
data Attribute = Attr Text Text
