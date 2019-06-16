{-# LANGUAGE OverloadedStrings #-}
module Stylish.Style.Interpret(
        compile, SelectorFunc(..)
    ) where

import Stylish.Parse.Selector

import Data.Text.Internal (Text(..))
import Data.List

data SelectorFunc = TestTag Text SelectorFunc | TestAttrs AttrsFunc SelectorFunc | Matched
data AttrsFunc = TestAttr Text PropertyTest AttrsFunc | MatchedAttrs

compile :: Selector -> SelectorFunc
compile (Element selector) = compileInner selector

compileInner selector = compileInner' $ lowerInner selector
compileInner' (Just tag, attributes) = TestTag tag $ TestAttrs (compileAttrs $ sortAttrs attributes) Matched
compileInner' (Nothing, attributes) = TestAttrs (compileAttrs $ sortAttrs attributes) Matched
compileAttrs ((name, test):attrs) = TestAttr name test $ compileAttrs attrs
compileAttrs [] = MatchedAttrs

lowerInner :: [SimpleSelector] -> (Maybe Text, [(Text, PropertyTest)])
lowerInner (Tag tag:selector) = (Just tag, snd $ lowerInner selector)
lowerInner (Id id:s) = (tag, ("id", Include id):tail) where (tag, tail) = lowerInner s
lowerInner (Class c:s) = (tag, ("class", Include c):tail) where (tag, tail) = lowerInner s
lowerInner (Property name test:s) = (tag, (name, test):tail) where (tag, tail) = lowerInner s
lowerInner [] = (Nothing, [])

sortAttrs = sortBy compareAttrs where compareAttrs x y = fst x `compare` fst y
