{-# LANGUAGE OverloadedStrings #-}
module Data.CSS.Style.Selector.Index (
        StyleIndex(..),
        rulesForElement
    ) where

-- TODO do performance tests to decide beside between strict/lazy.
import Data.HashMap.Strict
import Data.List (nub)
import Data.CSS.Style.Common

import Data.Hashable
import Data.Text (unpack, pack)
import Data.CSS.Syntax.Tokens (serialize) -- for easy hashing

data StyleIndex = StyleIndex {
    indexed :: HashMap SimpleSelector [StyleRule'],
    unindexed :: [StyleRule']
}

lookup' :: SimpleSelector -> HashMap SimpleSelector [a] -> [a]
lookup' = lookupDefault []

instance RuleStore StyleIndex where
    new = StyleIndex {indexed = empty, unindexed = []}
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

simpleSelector :: Selector -> [SimpleSelector]
simpleSelector (Element s) = s
simpleSelector (Child _ s) = s
simpleSelector (Descendant _ s) = s
simpleSelector (Adjacent _ s) = s
simpleSelector (Sibling _ s) = s

addRuleForSelector :: StyleIndex -> StyleRule' -> [SimpleSelector] -> StyleIndex
addRuleForSelector self@(StyleIndex index _) rule sel
  | Just key <- selectorKey sel = self {
        indexed = insert key (rule : lookup' key index) index
    }
  | otherwise = self {unindexed = rule : unindexed self}

selectorKey :: [SimpleSelector] -> Maybe SimpleSelector
selectorKey (tok@(Tag _) : _) = Just tok
selectorKey (tok@(Id _) : _) = Just tok
selectorKey (tok@(Class _) : _) = Just tok
selectorKey (Property prop _ : _) = Just $ Property prop Exists
selectorKey (_ : tokens) = selectorKey tokens
selectorKey [] = Nothing

----

testsForAttributes :: [Attribute] -> [SimpleSelector]
testsForElement :: Element -> [SimpleSelector]
testsForElement element =
    (Tag $ name element) : (testsForAttributes $ attributes element)
testsForAttributes (Attribute "class" value:attrs) =
    (Prelude.map (\s -> Class $ pack s) $ words value) ++
        (Property "class" Exists : testsForAttributes attrs)
testsForAttributes (Attribute "id" value:attrs) =
    (Prelude.map (\s -> Id $ pack s) $ words value) ++
        (Property "id" Exists : testsForAttributes attrs)
testsForAttributes (Attribute elName _:attrs) =
    Property elName Exists : testsForAttributes attrs
testsForAttributes [] = []

-- Implement hashable for SimpleSelector here because it proved challenging to automatically derive it.
instance Hashable SimpleSelector where
    hashWithSalt seed (Tag tag) = seed `hashWithSalt` (0::Int) `hashWithSalt` unpack tag
    hashWithSalt seed (Id i) = seed `hashWithSalt` (1::Int) `hashWithSalt` unpack i
    hashWithSalt seed (Class class_) = seed `hashWithSalt` (2::Int) `hashWithSalt` unpack class_
    hashWithSalt seed (Property prop test) =
        seed `hashWithSalt` (3::Int) `hashWithSalt` unpack prop `hashWithSalt` test
    hashWithSalt seed (Psuedoclass p args) =
        seed `hashWithSalt` (4::Int) `hashWithSalt` p `hashWithSalt` serialize args
    hashWithSalt seed (Psuedoelement p) = seed `hashWithSalt` (5::Int) `hashWithSalt` p

instance Hashable PropertyTest where
    hashWithSalt seed Exists = seed `hashWithSalt` (0::Int)
    hashWithSalt seed (Equals val) = seed `hashWithSalt` (1::Int) `hashWithSalt` unpack val
    hashWithSalt seed (Suffix val) = seed `hashWithSalt` (2::Int) `hashWithSalt` unpack val
    hashWithSalt seed (Prefix val) = seed `hashWithSalt` (3::Int) `hashWithSalt` unpack val
    hashWithSalt seed (Substring val) = seed `hashWithSalt` (4::Int) `hashWithSalt` unpack val
    hashWithSalt seed (Include val) = seed `hashWithSalt` (5::Int) `hashWithSalt` unpack val
    hashWithSalt seed (Dash val) = seed `hashWithSalt` (6::Int) `hashWithSalt` unpack val
