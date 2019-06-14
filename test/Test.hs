{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Stylish.Parse
import Data.CSS.Syntax.Tokens

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
                StyleRule [[Tag "bedroom"]] [
                    ("drapes", [Ident "blue"]),
                    ("carpet", [Ident "wool", Ident "shag"])
                ]]
            parse emptyStyle "  bathroom{tile :1in white;drapes :pink}" `shouldBe` TrivialStyleSheet [
                StyleRule [[Tag "bathroom"]] [
                    ("tile", [Dimension "1" (NVInteger 1) "in", Ident "white"]),
                    ("drapes", [Ident "pink"])
                ]]
        it "Parses selectors" $ do
            parse emptyStyle ".class {}" `shouldBe` TrivialStyleSheet [
                    StyleRule [[Class "class"]] []
                ]
            parse emptyStyle "*.class {}" `shouldBe` TrivialStyleSheet [
                    StyleRule [[Class "class"]] []
                ]
            parse emptyStyle "#id {}" `shouldBe` TrivialStyleSheet [
                    StyleRule [[Id "id"]] []
                ]
            parse emptyStyle "[attr] {}" `shouldBe` TrivialStyleSheet [
                    StyleRule [[Property "attr" Exists]] []
                ]
            parse emptyStyle "a , b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule [[Tag "a"], [Tag "b"]] []
                ]
            parse emptyStyle "a b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule [[Tag "a", Descendant, Tag "b"]] []
                ]
            parse emptyStyle "a > b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule [[Tag "a", Child, Tag "b"]] []
                ]
            parse emptyStyle "a ~ b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule [[Tag "a", Sibling, Tag "b"]] []
                ]
            parse emptyStyle "a + b {}" `shouldBe` TrivialStyleSheet [
                    StyleRule [[Tag "a", Adjacent, Tag "b"]] []
                ]

emptyStyle = TrivialStyleSheet []
linkStyle = TrivialStyleSheet [
        StyleRule [[Tag "a"]] [("color", [Ident "green"])]
    ]
