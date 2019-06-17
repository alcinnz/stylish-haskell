{-# LANGUAGE OverloadedStrings #-}
module Stylish.Style.Selector.Index (
        StyleIndex(..), styleIndex,
        rulesForElement
    ) where

-- TODO do performance tests to decide beside between strict/lazy.
import Data.HashMap.Strict
import Data.List (nub)
import Stylish.Parse
import Stylish.Element
import Stylish.Style.Selector.Common

import Data.Hashable
import Data.Text (unpack, pack)
import Data.Text.Internal (Text(..))

data StyleIndex = StyleIndex {
    indexed :: HashMap SimpleSelector [StyleRule'],
    unindexed :: [StyleRule']
}

styleIndex = StyleIndex {indexed = empty, unindexed = []}

lookup' :: SimpleSelector -> HashMap SimpleSelector [a] -> [a]
lookup' = lookupDefault []

instance RuleStore StyleIndex where
    addStyleRule self _ rule | [] == properties rule = self
        | otherwise = addRuleForSelector self rule $ simpleSelector $ selector rule
    lookupRules self element = nub $ Prelude.foldr (++) [] rules
        where
            get key = lookup' key index
            index = indexed self
            rules = unindexed self : Prelude.map get (testsForElement element)

rulesForElement :: StyleIndex -> Element -> [StyleRule] -- For testing
rulesForElement self element = Prelude.map inner $ lookupRules self element

---

simpleSelector (Element s) = s
simpleSelector (Child _ s) = s
simpleSelector (Descendant _ s) = s
simpleSelector (Adjacent _ s) = s
simpleSelector (Sibling _ s) = s

addRuleForSelector self rule [] = self {unindexed = rule : unindexed self}
addRuleForSelector self rule selector = self {
        indexed = insert key (rule : lookup' key index) index
    } where
        key = selectorKey selector
        index = indexed self

selectorKey (tok@(Tag _) : _) = tok
selectorKey (tok@(Id _) : _) = tok
selectorKey (tok@(Class _) : _) = tok
selectorKey (Property prop _ : _) = Property prop Exists

----

testsForElement :: Element -> [SimpleSelector]
testsForElement element =
    (Tag $ name element) : (testsForAttributes $ attributes element)
testsForAttributes (Attribute "class" value:attrs) =
    (Prelude.map (\s -> Class $ pack s) $ words value) ++
        (Property "class" Exists : testsForAttributes attrs)
testsForAttributes (Attribute "id" value:attrs) =
    (Prelude.map (\s -> Id $ pack s) $ words value) ++
        (Property "id" Exists : testsForAttributes attrs)
testsForAttributes (Attribute name _:attrs) =
    Property name Exists : testsForAttributes attrs
testsForAttributes [] = []

-- Implement hashable for SimpleSelector here because it proved challenging to automatically derive it.
instance Hashable SimpleSelector where
    hashWithSalt seed (Tag tag) = seed `hashWithSalt` (0::Int) `hashWithSalt` unpack tag
    hashWithSalt seed (Id id) = seed `hashWithSalt` (1::Int) `hashWithSalt` unpack id
    hashWithSalt seed (Class class_) = seed `hashWithSalt` (2::Int) `hashWithSalt` unpack class_
    hashWithSalt seed (Property prop test) =
        seed `hashWithSalt` (3::Int) `hashWithSalt` unpack prop `hashWithSalt` test

instance Hashable PropertyTest where
    hashWithSalt seed Exists = seed `hashWithSalt` (0::Int)
    hashWithSalt seed (Equals val) = seed `hashWithSalt` (1::Int) `hashWithSalt` unpack val
    hashWithSalt seed (Suffix val) = seed `hashWithSalt` (2::Int) `hashWithSalt` unpack val
    hashWithSalt seed (Prefix val) = seed `hashWithSalt` (3::Int) `hashWithSalt` unpack val
    hashWithSalt seed (Substring val) = seed `hashWithSalt` (4::Int) `hashWithSalt` unpack val
    hashWithSalt seed (Include val) = seed `hashWithSalt` (5::Int) `hashWithSalt` unpack val
    hashWithSalt seed (Dash val) = seed `hashWithSalt` (6::Int) `hashWithSalt` unpack val
