{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.HashMap.Strict
import Data.Maybe (fromJust)

import Data.CSS.Syntax.Tokens
import Data.CSS.Syntax.StyleSheet
import Data.CSS.Syntax.Selector

import Data.CSS.Style.Common
import Data.CSS.Style.Selector.Index
import Data.CSS.Style.Selector.Interpret
import Data.CSS.Style

import Data.CSS.Preprocessor.Conditions

main :: IO ()
main = hspec spec

spec :: Spec
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
                ] ""]
            parse emptyStyle "  bathroom{tile :1in white;drapes :pink}" `shouldBe` TrivialStyleSheet [
                StyleRule (Element [Tag "bathroom"]) [
                    ("tile", [Dimension "1" (NVInteger 1) "in", Ident "white"]),
                    ("drapes", [Ident "pink"])
                ] ""]
        it "Parses selectors" $ do
            parse emptyStyle ".class {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Class "class"]) [] ""
                ]
            parse emptyStyle "*.class {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Class "class"]) [] ""
                ]
            parse emptyStyle "#id {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Id "id"]) [] ""
                ]
            parse emptyStyle "[attr] {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Property "attr" Exists]) [] ""
                ]
            parse emptyStyle "a , b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Tag "b"]) [] "",
                    StyleRule (Element [Tag "a"]) [] ""
                ]
            parse emptyStyle "a b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Descendant (Element [Tag "a"]) [Tag "b"]) [] ""
                ]
            parse emptyStyle "a > b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Child (Element [Tag "a"]) [Tag "b"]) [] ""
                ]
            parse emptyStyle "a ~ b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Sibling (Element [Tag "a"]) [Tag "b"]) [] ""
                ]
            parse emptyStyle "a + b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule (Adjacent (Element [Tag "a"]) [Tag "b"]) [] ""
                ]
            parse emptyStyle "a::before {}"
                `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Tag "a"]) [] "before"
                ]
            parse emptyStyle "a:before {}"
                `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Tag "a", Psuedoclass "before" []]) [] ""
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

            let rule1 = StyleRule (Element [Class "external"]) [("color", [Ident "green"])] ""
            let index1 = addStyleRule styleIndex 0 $ styleRule' rule1
            rulesForElement index1 element `shouldBe` [rule1]
            rulesForElement index1 element2 `shouldBe` []

            let rule2 = StyleRule (Element [Id "mysite"]) [("color", [Ident "green"])] ""
            let index2 = addStyleRule styleIndex 0 $ styleRule' rule2
            rulesForElement index2 element `shouldBe` [rule2]
            rulesForElement index2 element2 `shouldBe` []

            let rule3 = StyleRule (Element [Property "href" $ Prefix "https://"]) [("color", [Ident "green"])] ""
            let index3 = addStyleRule styleIndex 0 $ styleRule' rule3
            rulesForElement index3 element `shouldBe` [rule3]
            rulesForElement index3 element2 `shouldBe` []
    describe "Selector Compiler" $ do
        it "Correctly evaluates selectors" $ do
            let parentEl = ElementNode {
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
                parent = Just parentEl,
                previous = Nothing,
                attributes = []
            }
            let child = ElementNode {
                name = "b",
                parent = Just parentEl,
                previous = Just sibling,
                attributes = []
            }

            let selector1 = compile (Element [Tag "a"])
            selector1 parentEl `shouldBe` True
            selector1 sibling `shouldBe` False
            selector1 child `shouldBe` False

            let selector2 = compile (Element [Class "external"])
            selector2 parentEl `shouldBe` True
            selector2 sibling `shouldBe` False
            selector2 child `shouldBe` False

            let selector3 = compile (Element [Id "mysite"])
            selector3 parentEl `shouldBe` True
            selector3 sibling `shouldBe` False
            selector3 child `shouldBe` False

            let selector4 = compile (Element [Property "lang" Exists])
            selector4 parentEl `shouldBe` True
            selector4 sibling `shouldBe` False
            selector4 child `shouldBe` False

            let selector5 = compile (Element [Property "class" $ Include "secure"])
            selector5 parentEl `shouldBe` True
            selector5 sibling `shouldBe` False
            selector5 child `shouldBe` False

            let selector6 = compile (Element [Property "href" $ Prefix "https://"])
            selector6 parentEl `shouldBe` True
            selector6 sibling `shouldBe` False
            selector6 child `shouldBe` False

            let selector7 = compile (Element [Property "href" $ Suffix ".html"])
            selector7 parentEl `shouldBe` True
            selector7 sibling `shouldBe` False
            selector7 child `shouldBe` False

            let selector8 = compile (Element [Property "href" $ Substring ".geek.nz"])
            selector8 parentEl `shouldBe` True
            selector8 sibling `shouldBe` False
            selector8 child `shouldBe` False

            let selector9 = compile (Element [Property "lang" $ Dash "en"])
            selector9 parentEl `shouldBe` True
            selector9 sibling `shouldBe` False
            selector9 child `shouldBe` False

            let selectorA = compile (Element [Property "lang" $ Dash "en-US"])
            selectorA parentEl `shouldBe` True
            selectorA sibling `shouldBe` False
            selectorA child `shouldBe` False

            let selectorB = compile (Element [Property "lang" $ Dash "en-UK"])
            selectorB parentEl `shouldBe` False
            selectorB sibling `shouldBe` False
            selectorB child `shouldBe` False

            -- TODO These could be tested better.
            let selectorC = compile $ Child (Element [Tag "a"]) [Tag "b"]
            selectorC parentEl `shouldBe` False
            selectorC sibling `shouldBe` False
            selectorC child `shouldBe` True

            let selectorD = compile $ Descendant (Element [Tag "a"]) [Tag "b"]
            selectorD parentEl `shouldBe` False
            selectorD sibling `shouldBe` False
            selectorD child `shouldBe` True

            let selectorE = compile $ Sibling (Element [Tag "img"]) [Tag "b"]
            selectorE parentEl `shouldBe` False
            selectorE sibling `shouldBe` False
            selectorE child `shouldBe` True

            let selectorF = compile $ Adjacent (Element [Tag "img"]) [Tag "b"]
            selectorF parentEl `shouldBe` False
            selectorF sibling `shouldBe` False
            selectorF child `shouldBe` True

    describe "Style resolution" $ do
        it "respects selector specificity" $ do
            let el = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "link"]
            }
            let rules = parse queryable "a.link {color: green} a {color: red}"
            let VarParser _ (TrivialPropertyParser style) = cascade rules el [] temp::(VarParser TrivialPropertyParser)
            style ! "color" `shouldBe` [Ident "green"]
        it "respects syntax order" $ do
            let el = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "link"]
            }
            let rules = parse queryable "a {color: red; color: green}"
            let VarParser _ (TrivialPropertyParser style) = cascade rules el [] temp::(VarParser TrivialPropertyParser)
            style ! "color" `shouldBe` [Ident "green"]

            let rules2 = parse queryable "a {color: red} a {color: green}"
            let VarParser _ (TrivialPropertyParser style2) = cascade rules2 el [] temp::(VarParser TrivialPropertyParser)
            style2 ! "color" `shouldBe` [Ident "green"]
        it "respects stylesheet precedence" $ do
            let el = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "link"]
            }
            let rules = parse (queryable {priority = 1}) "a {color: green}"
            let rules2 = parse (rules {priority = 2}) "a {color: red}" :: QueryableStyleSheet (VarParser TrivialPropertyParser)
            let VarParser _ (TrivialPropertyParser style) = cascade rules2 el [] temp::(VarParser TrivialPropertyParser)
            style ! "color" `shouldBe` [Ident "green"]

            let el' = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "link"]
            }
            let rules' = parse (queryable {priority = 1}) "a {color: red}"
            let rules2' = parse (rules' {priority = 2}) "a {color: green !important}" :: QueryableStyleSheet (VarParser TrivialPropertyParser)
            let VarParser _ (TrivialPropertyParser style') = cascade rules2' el' [] temp::(VarParser TrivialPropertyParser)
            style' ! "color" `shouldBe` [Ident "green"]
        it "respects overrides" $ do
            let el = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = [Attribute "class" "link"]
            }
            let rules = parse queryable "a {color: red;}"
            let VarParser _ (TrivialPropertyParser style) = cascade rules el [("color", [Ident "green"])] temp::(VarParser TrivialPropertyParser)
            style ! "color" `shouldBe` [Ident "green"]
    describe "Parser freezes" $ do
        it "does not regress" $ do
            parse emptyStyle "output: {content: 'Output'; pitch: high}"
                `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Tag "output"]) [] ""
                ] -- Turned out to just be incorrect parsing
            parse emptyStyle "input, output {content: attr(value)}"
                `shouldBe` TrivialStyleSheet [
                    StyleRule (Element [Tag "output"]) [
                        ("content", [Function "attr", Ident "value", RightParen])
                    ] "",
                    StyleRule (Element [Tag "input"]) [
                        ("content", [Function "attr", Ident "value", RightParen])
                    ] ""
                ]
        it "paren balancing" $ do
            scanValue [RightParen] `shouldBe` ([], [RightParen])
            scanValue [LeftParen] `shouldBe` ([LeftParen], [])
            scanValue [Function "fn", LeftParen] `shouldBe` ([Function "fn", LeftParen], [])
            scanValue [Function "fn", Ident "arg", LeftParen] `shouldBe`
                ([Function "fn", Ident "arg", LeftParen], [])

    describe "CSS Variables" $ do
        it "are captured" $ do
            let parser = temp :: VarParser TrivialPropertyParser
            vars parser `shouldBe` []
            let parser1 = setVars [("--var", [Ident "value"])] parser
            vars parser1 `shouldBe` [("--var", [Ident "value"])]
            let parser2 = fromJust $ longhand parser parser1 "property" [Function "var", Ident "--var", RightParen]
            vars parser2 `shouldBe` [("--var", [Ident "value"])]
            let VarParser _ (TrivialPropertyParser style) = parser2
            style ! "property" `shouldBe` [Ident "value"]

            let el = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = []
            }
            let rules = parse queryable "a {--var: value}"
            let VarParser v _ = cascade rules el [] temp
            v `shouldBe` [("--var", [Ident "value"])]
        it "applies within element" $ do
            let el = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = []
            }
            let rules = parse queryable "a {--link: #f00; color: var(--link)}"
            let VarParser vars (TrivialPropertyParser style) = cascade rules el [] temp
            style ! "color" `shouldBe` [Hash HId "f00"]
            style ! "--link" `shouldBe` [Hash HId "f00"]
            vars `shouldBe` [("--link", [Hash HId "f00"])]
        it "inherits" $ do
            let parent = ElementNode {
                name = "a",
                parent = Nothing,
                previous = Nothing,
                attributes = []
            }
            let el = ElementNode {
                name = "b",
                parent = Just parent,
                previous = Nothing,
                attributes = []
            }
            let rules = parse queryable "a {--link: #f00} b {color: var(--link)}"
            let VarParser vars (TrivialPropertyParser style) = cascade rules el [] $ cascade rules parent [] temp
            vars `shouldBe` [("--link", [Hash HId "f00"])]
            style ! "color" `shouldBe` [Hash HId "f00"]

styleIndex :: StyleIndex
styleIndex = new
queryable :: QueryableStyleSheet (VarParser TrivialPropertyParser)
queryable = queryableStyleSheet
emptyStyle :: TrivialStyleSheet
emptyStyle = TrivialStyleSheet []
linkStyle :: TrivialStyleSheet
linkStyle = TrivialStyleSheet [sampleRule]
sampleRule :: StyleRule
sampleRule = StyleRule (Element [Tag "a"]) [("color", [Ident "green"])] ""
