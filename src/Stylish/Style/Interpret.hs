{-# LANGUAGE OverloadedStrings #-}
module Stylish.Style.Interpret(
        compile, SelectorFunc(..)
    ) where

import Stylish.Parse.Selector
import Stylish.Element

import Data.Text.Internal (Text(..))
import Data.Text (unpack)
import Data.List
import Data.Maybe

type SelectorFunc = Element -> Bool
type AttrsFunc = [Attribute] -> Bool

compile :: Selector -> SelectorFunc
compile (Element selector) = compileInner selector
compile (Child upSelector selector) = direct parent (compile upSelector) $ compileInner selector
compile (Descendant up sel) = indirect parent (compile up) $ compileInner sel
compile (Adjacent up sel) = direct previous (compile up) $ compileInner sel
compile (Sibling up sel) = indirect previous (compile up) $ compileInner sel

compileInner selector = compileInner' $ lowerInner selector
compileInner' :: (Maybe Text, [(Text, String -> Bool)]) -> SelectorFunc
compileInner' (Just tag, attributes) = testTag tag $ testAttrs (compileAttrs $ sortAttrs attributes) matched
compileInner' (Nothing, attributes) = testAttrs (compileAttrs $ sortAttrs attributes) matched
compileAttrs :: [(Text, String -> Bool)] -> AttrsFunc
compileAttrs ((name, test):attrs) = testAttr name test $ compileAttrs attrs
compileAttrs [] = matched

lowerInner (Tag tag:selector) = (Just tag, snd $ lowerInner selector)
lowerInner (Id id:s) = (tag, ("id", hasWord $ unpack id):tail) where (tag, tail) = lowerInner s
lowerInner (Class c:s) = (tag, ("class", hasWord $ unpack c):tail) where (tag, tail) = lowerInner s
lowerInner (Property name test:s) = (tag, (name, compileAttrTest test):tail)
    where (tag, tail) = lowerInner s
lowerInner [] = (Nothing, [])

compileAttrTest Exists = matched
compileAttrTest (Equals val) = (== (unpack val))
compileAttrTest (Suffix val) = isSuffixOf $ unpack val
compileAttrTest (Prefix val) = isPrefixOf $ unpack val
compileAttrTest (Substring val) = isInfixOf $ unpack val
compileAttrTest (Include val) = hasWord $ unpack val
compileAttrTest (Dash val) = hasLang $ unpack val

sortAttrs = sortBy compareAttrs where compareAttrs x y = fst x `compare` fst y

--------
---- Runtime
--------
testTag :: Text -> SelectorFunc -> SelectorFunc
testTag tag success el | name el == tag = success el
    | otherwise = False
testAttrs :: AttrsFunc -> SelectorFunc -> SelectorFunc
testAttrs attrsTest success el | attrsTest $ attributes el = success el
    | otherwise = False
direct traverser upTest test el
    | Just parent <- traverser el = test el && upTest parent
    | otherwise = False
indirect :: (Element -> Maybe Element) -> SelectorFunc -> SelectorFunc -> SelectorFunc
indirect traverser upTest test el | Nothing <- traverser el = False
    | not $ test el = False
    | upTest (fromJust $ traverser el) = True
    | otherwise = indirect traverser upTest test $ fromJust $ traverser el
matched _ = True

testAttr :: Text -> (String -> Bool) -> AttrsFunc -> AttrsFunc
testAttr expected test next attrs@(Attribute name value : attrs')
    | name < expected = testAttr expected test next attrs'
    | name > expected = False
    | name == expected && test value = next attrs
    | otherwise = False
testAttr _ _ _ [] = False

hasWord expected value = expected `elem` words value
hasLang expected value = expected == value || isPrefixOf (expected ++ "-") value
