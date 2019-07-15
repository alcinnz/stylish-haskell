{-# LANGUAGE OverloadedStrings #-}
module Data.CSS.Style.Selector.Interpret(
        compile, SelectorFunc,
        InterpretedRuleStore(..)
    ) where

import Data.CSS.Style.Common

import Data.Text (unpack)
import Data.List
import Data.Maybe

type SelectorFunc = Element -> Bool
type AttrsFunc = [Attribute] -> Bool

compile :: Selector -> SelectorFunc
compile (Element sel) = compileInner sel
compile (Child upSel sel) = direct parent (compile upSel) $ compileInner sel
compile (Descendant up sel) = indirect parent (compile up) $ compileInner sel
compile (Adjacent up sel) = direct previous (compile up) $ compileInner sel
compile (Sibling up sel) = indirect previous (compile up) $ compileInner sel

compileInner :: [SimpleSelector] -> SelectorFunc
compileInner sel = compileInner' $ lowerInner sel
compileInner' :: (Maybe Text, [(Text, String -> Bool)]) -> SelectorFunc
compileInner' (Just tag, attrs) = testTag tag $ testAttrs (compileAttrs $ sortAttrs attrs) matched
compileInner' (Nothing, attrs) = testAttrs (compileAttrs $ sortAttrs attrs) matched
compileAttrs :: [(Text, String -> Bool)] -> AttrsFunc
compileAttrs ((tag, test):attrs) = testAttr tag test $ compileAttrs attrs
compileAttrs [] = matched

lowerInner :: [SimpleSelector] -> (Maybe Text, [(Text, String -> Bool)])
lowerInner (Tag tag:sel) = (Just tag, snd $ lowerInner sel)
lowerInner (Id i:s) = (tag, ("id", hasWord $ unpack i):attrs) where (tag, attrs) = lowerInner s
lowerInner (Class c:s) = (tag, ("class", hasWord $ unpack c):attrs) where (tag, attrs) = lowerInner s
lowerInner (Property prop test:s) = (tag, (prop, compileAttrTest test):attrs)
    where (tag, attrs) = lowerInner s
lowerInner [] = (Nothing, [])

compileAttrTest :: PropertyTest -> String -> Bool
compileAttrTest Exists = matched
compileAttrTest (Equals val) = (== (unpack val))
compileAttrTest (Suffix val) = isSuffixOf $ unpack val
compileAttrTest (Prefix val) = isPrefixOf $ unpack val
compileAttrTest (Substring val) = isInfixOf $ unpack val
compileAttrTest (Include val) = hasWord $ unpack val
compileAttrTest (Dash val) = hasLang $ unpack val

sortAttrs :: [(Text, b)] -> [(Text, b)]
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
direct :: (Element -> Maybe Element) -> SelectorFunc -> SelectorFunc -> SelectorFunc
direct traverser upTest test el | Just up <- traverser el = test el && upTest up
    | otherwise = False
indirect :: (Element -> Maybe Element) -> SelectorFunc -> SelectorFunc -> SelectorFunc
indirect traverser upTest test el | Nothing <- traverser el = False
    | not $ test el = False
    | upTest (fromJust $ traverser el) = True
    | otherwise = indirect traverser upTest test $ fromJust $ traverser el
matched :: t -> Bool
matched _ = True

testAttr :: Text -> (String -> Bool) -> AttrsFunc -> AttrsFunc
testAttr expected test next attrs@(Attribute attr value : attrs')
    | attr < expected = testAttr expected test next attrs'
    | attr > expected = False
    | attr == expected && test value = next attrs
    | otherwise = False
testAttr _ _ _ [] = False

hasWord :: String -> String -> Bool
hasWord expected value = expected `elem` words value
hasLang :: [Char] -> [Char] -> Bool
hasLang expected value = expected == value || isPrefixOf (expected ++ "-") value

--------
---- RuleStore wrapper
--------
data InterpretedRuleStore inner = InterpretedRuleStore inner
instance RuleStore inner => RuleStore (InterpretedRuleStore inner) where
    new = InterpretedRuleStore new
    addStyleRule (InterpretedRuleStore self) priority rule =
        InterpretedRuleStore $ addStyleRule self priority $ rule {
            compiledSelector = compile $ selector rule
        }
    lookupRules (InterpretedRuleStore self) el = filter call $ lookupRules self el
        where call (StyleRule' _ test _) = test el
