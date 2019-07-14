{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.HashMap.Strict

import Data.CSS.Syntax.Tokens
import Data.CSS.Syntax.StyleSheet
import Data.CSS.Syntax.Selector

import Data.CSS.Style.Common
import Data.CSS.Style.Selector.Index
import Data.CSS.Style.Selector.Interpret
import Data.CSS.Style

main :: IO ()
main = hspec spec

spec = do
    describe "Canary" $ do
        it "Test framework works" $ do
            True `shouldBe` True
    describe "Parsing" $ do
        it "Ignores @rules" $ do
            parse emptyStyle "@encoding 'utf-8';" `shouldBe` emptyStyle
            parse emptyStyle "  @encoding 'utf-8';" `shouldBe` emptyStyle
            parse emptyStyle "@encoding 'utf-8';  " `shouldBe` emptyStyle
            parse emptyStyle "@media print { a:link {color: green;} }" `shouldBe` emptyStyle
            parse emptyStyle "  @media print { a:link {color: green;} }" `shouldBe` emptyStyle
            parse emptyStyle "@media print { a:link {color: green;} }  " `shouldBe` emptyStyle

            parse emptyStyle "@encoding 'utf-8'; a {color:green}" `shouldBe` linkStyle
            parse emptyStyle "a {color:green}@encoding 'utf-8';" `shouldBe` linkStyle
            parse emptyStyle "@media print{a{color:black;}}a {color:green}" `shouldBe` linkStyle
            parse emptyStyle "a {color:green} @media print {a{color:black;}}" `shouldBe` linkStyle
        it "Parses style rules" $ do
            -- Syntax examples from "Head First HTML & CSS with XHTML"
            parse emptyStyle "bedroom { drapes: blue; carpet: wool shag; }" `shouldBe` TrivialStyleSheet [
                StyleRule (Element [Tag "bedroom"]) [
                    ("drapes", [Ident "blue"]),
                    ("carpet", [Ident "wool", Ident "shag"])
                ]]
            parse emptyStyle "  bathroom{tile :1in white;drapes :pink}" `shouldBe` TrivialStyleSheet [
                StyleRule (Element [Tag "bathroom"]) [
                    ("tile", [Dimension "1" (NVInteger 1) "in", Ident "white"]),
                    ("drapes", [Ident "pink"])
                ]]
        it "Parses selectors" $ do
            parse emptyStyle ".class {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Class "class"]) []
                ]
            parse emptyStyle "*.class {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Class "class"]) []
                ]
            parse emptyStyle "#id {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Id "id"]) []
                ]
            parse emptyStyle "[attr] {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Property "attr" Exists]) []
                ]
            parse emptyStyle "a , b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Tag "b"]) [],
                    StyleRule (Element [Tag "a"]) []
                ]
            parse emptyStyle "a b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Descendant (Element [Tag "a"]) [Tag "b"]) []
                ]
            parse emptyStyle "a > b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Child (Element [Tag "a"]) [Tag "b"]) []
                ]
            parse emptyStyle "a ~ b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Sibling (Element [Tag "a"]) [Tag "b"]) []
                ]
            parse emptyStyle "a + b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Adjacent (Element [Tag "a"]) [Tag "b"]) []
                ]
    describe "Style Index" $ do
        it "Retrieves appropriate styles" $ do
            let index = addStyleRule styleIndex 0 $ styleRule' sampleRule
            let element = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = [
                    Attribute "class" "external",
                    Attribute "href" "https://adrian.geek.nz/",
                    Attribute "id" "mysite"
                ]
            }
            let element2 = ElementNode {
                name = "b",
                parent = Just element,
                previous = Just element, -- Invalid tree, oh well.
                attributes = []
            }
            rulesForElement index element `shouldBe` [sampleRule]
            rulesForElement index element2 `shouldBe` []

            let rule = StyleRule (Element [Class "external"]) [("color", [Ident "green"])]
            let index = addStyleRule styleIndex 0 $ styleRule' rule
            rulesForElement index element `shouldBe` [rule]
            rulesForElement index element2 `shouldBe` []

            let rule = StyleRule (Element [Id "mysite"]) [("color", [Ident "green"])]
            let index = addStyleRule styleIndex 0 $ styleRule' rule
            rulesForElement index element `shouldBe` [rule]
            rulesForElement index element2 `shouldBe` []

            let rule = StyleRule (Element [Property "href" $ Prefix "https://"]) [("color", [Ident "green"])]
            let index = addStyleRule styleIndex 0 $ styleRule' rule
            rulesForElement index element `shouldBe` [rule]
            rulesForElement index element2 `shouldBe` []
    describe "Selector Compiler" $ do
        it "Correctly evaluates selectors" $ do
            let parent = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = [
                    Attribute "class" "external secure link",
                    Attribute "href" "https://adrian.geek.nz/index.html",
                    Attribute "id" "mysite",
                    Attribute "lang" "en-US"
                ]
            }
            let sibling = ElementNode {
                name = "img",
                parent = Just parent,
                previous = Nothing,
                attributes = []
            }
            let child = ElementNode {
                name = "b",
                parent = Just parent,
                previous = Just sibling,
                attributes = []
            }

            let selector = compile (Element [Tag "a"])
            selector parent `shouldBe` True
            selector sibling `shouldBe` False
            selector child `shouldBe` False

            let selector = compile (Element [Class "external"])
            selector parent `shouldBe` True
            selector sibling `shouldBe` False
            selector child `shouldBe` False

            let selector = compile (Element [Id "mysite"])
            selector parent `shouldBe` True
            selector sibling `shouldBe` False
            selector child `shouldBe` False

            let selector = compile (Element [Property "lang" Exists])
            selector parent `shouldBe` True
            selector sibling `shouldBe` False
            selector child `shouldBe` False

            let selector = compile (Element [Property "class" $ Include "secure"])
            selector parent `shouldBe` True
            selector sibling `shouldBe` False
            selector child `shouldBe` False

            let selector = compile (Element [Property "href" $ Prefix "https://"])
            selector parent `shouldBe` True
            selector sibling `shouldBe` False
            selector child `shouldBe` False

            let selector = compile (Element [Property "href" $ Suffix ".html"])
            selector parent `shouldBe` True
            selector sibling `shouldBe` False
            selector child `shouldBe` False

            let selector = compile (Element [Property "href" $ Substring ".geek.nz"])
            selector parent `shouldBe` True
            selector sibling `shouldBe` False
            selector child `shouldBe` False

            let selector = compile (Element [Property "lang" $ Dash "en"])
            selector parent `shouldBe` True
            selector sibling `shouldBe` False
            selector child `shouldBe` False

            let selector = compile (Element [Property "lang" $ Dash "en-US"])
            selector parent `shouldBe` True
            selector sibling `shouldBe` False
            selector child `shouldBe` False

            let selector = compile (Element [Property "lang" $ Dash "en-UK"])
            selector parent `shouldBe` False
            selector sibling `shouldBe` False
            selector child `shouldBe` False

            -- TODO These could be tested better.
            let selector = compile $ Child (Element [Tag "a"]) [Tag "b"]
            selector parent `shouldBe` False
            selector sibling `shouldBe` False
            selector child `shouldBe` True

            let selector = compile $ Descendant (Element [Tag "a"]) [Tag "b"]
            selector parent `shouldBe` False
            selector sibling `shouldBe` False
            selector child `shouldBe` True

            let selector = compile $ Sibling (Element [Tag "img"]) [Tag "b"]
            selector parent `shouldBe` False
            selector sibling `shouldBe` False
            selector child `shouldBe` True

            let selector = compile $ Adjacent (Element [Tag "img"]) [Tag "b"]
            selector parent `shouldBe` False
            selector sibling `shouldBe` False
            selector child `shouldBe` True

    describe "Style resolution" $ do
        it "respects selector specificity" $ do
            let el = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "link"]
            }
            let rules = parse queryable "a.link {color: green} a {color: red}"
            let TrivialPropertyParser style = cascade rules el [] temp::TrivialPropertyParser
            style ! "color" `shouldBe` [Ident "green"]
        it "respects syntax order" $ do
            let el = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "link"]
            }
            let rules = parse queryable "a {color: red; color: green}"
            let TrivialPropertyParser style = cascade rules el [] temp::TrivialPropertyParser
            style ! "color" `shouldBe` [Ident "green"]

            let rules = parse queryable "a {color: red} a {color: green}"
            let TrivialPropertyParser style = cascade rules el [] temp::TrivialPropertyParser
            style ! "color" `shouldBe` [Ident "green"]
        it "respects stylesheet precedence" $ do
            let el = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "link"]
            }
            let rules = parse (queryable {priority = 1}) "a {color: green}"
            let rules2 = parse (rules {priority = 2}) "a {color: red}" :: QueryableStyleSheet TrivialPropertyParser
            let TrivialPropertyParser style = cascade rules2 el [] temp::TrivialPropertyParser
            style ! "color" `shouldBe` [Ident "green"]

            let el = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "link"]
            }
            let rules = parse (queryable {priority = 1}) "a {color: red}"
            let rules2 = parse (rules {priority = 2}) "a {color: green !important}" :: QueryableStyleSheet TrivialPropertyParser
            let TrivialPropertyParser style = cascade rules2 el [] temp::TrivialPropertyParser
            style ! "color" `shouldBe` [Ident "green"]
        it "respects overrides" $ do
            let el = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "link"]
            }
            let rules = parse queryable "a {color: red;}"
            let TrivialPropertyParser style = cascade rules el [("color", [Ident "green"])] temp::TrivialPropertyParser
            style ! "color" `shouldBe` [Ident "green"]
    describe "Parser freezes" $ do
        it "does not regress" $ do
            -- TODO handle psuedoelements
            parse emptyStyle "output::before {content: 'Output'; pitch: high}"
                `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Tag "output"]) []
                ]

styleIndex :: StyleIndex
styleIndex = new
queryable :: QueryableStyleSheet TrivialPropertyParser
queryable = queryableStyleSheet
emptyStyle = TrivialStyleSheet []
linkStyle = TrivialStyleSheet [sampleRule]
sampleRule = StyleRule (Element [Tag "a"]) [("color", [Ident "green"])]
