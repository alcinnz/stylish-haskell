{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.CSS.Syntax.Tokens

import Stylish.Parse
import Stylish.Style.Index
import Stylish.Element

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
            let index = addRule styleIndex sampleRule
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
            let index = addRule styleIndex rule
            rulesForElement index element `shouldBe` [rule]
            rulesForElement index element2 `shouldBe` []

            let rule = StyleRule (Element [Id "mysite"]) [("color", [Ident "green"])]
            let index = addRule styleIndex rule
            rulesForElement index element `shouldBe` [rule]
            rulesForElement index element2 `shouldBe` []

            let rule = StyleRule (Element [Property "href" $ Prefix "https://"]) [("color", [Ident "green"])]
            let index = addRule styleIndex rule
            rulesForElement index element `shouldBe` [rule]
            rulesForElement index element2 `shouldBe` []

emptyStyle = TrivialStyleSheet []
linkStyle = TrivialStyleSheet [sampleRule]
sampleRule = StyleRule (Element [Tag "a"]) [("color", [Ident "green"])]
