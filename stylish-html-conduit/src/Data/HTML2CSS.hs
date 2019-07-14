{-# LANGUAGE OverloadedStrings #-}
module Data.HTML2CSS(
        externalStyles, internalStyles,
        cssPriorityAgent, cssPriorityUser, cssPriorityAuthor,
        traverseStyles, traverseStyles', elToStylish
    ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as Txt

import qualified Text.XML as XML
import Data.CSS.Syntax.StyleSheet
import Data.CSS.Style

import Network.URI

---- Constants
cssPriorityAgent = 1
cssPriorityUser = 2
cssPriorityAuthor = 3

---- Parsing
externalStyles :: PropertyParser s => QueryableStyleSheet s -> (M.Map XML.Name Txt.Text -> Bool) ->
        XML.Element -> (URI -> IO Txt.Text) -> IO (QueryableStyleSheet s)
externalStyles stylesheet testMedia html loadURL = do
    css <- externalStyles' testMedia html loadURL
    return $ foldl parse stylesheet {priority = cssPriorityAuthor} css
externalStyles' testMedia html loadURL = go $ linkedStyles' testMedia html
    where -- TODO parallelise loads
        go (link:links) = do
            response <- loadURL $ link
            rest <- go links
            return $ response : rest
        go [] = return []

linkedStyles' testMedia (XML.Element (XML.Name "link" _ _) attrs _)
    | Just link <- "href" `M.lookup` attrs,
        Just "stylesheet" <- "rel" `M.lookup` attrs,
        testMedia attrs,
        Just uri <- parseURIReference $ Txt.unpack link = [uri]
linkedStyles' testMedia (XML.Element _ _ children) =
    concat [linkedStyles' testMedia el | XML.NodeElement el <- children]

internalStyles testMedia stylesheet html =
    foldl parse (stylesheet {priority = cssPriorityAuthor}) $ internalStyles' testMedia html
internalStyles' testMedia (XML.Element (XML.Name "style"_ _) attrs children)
    | testMedia attrs = [strContent children]
internalStyles' testMedia (XML.Element _ _ children) =
    concat [internalStyles' testMedia el | XML.NodeElement el <- children]


strContent :: [XML.Node] -> Txt.Text
strContent (XML.NodeContent text : rest) = text `Txt.append` strContent rest
-- We do want to read in comments for CSS, just not for display.
strContent (XML.NodeComment text : rest) = text `Txt.append` strContent rest
strContent (XML.NodeElement (XML.Element _ _ children):rest) =
    strContent children `Txt.append` strContent rest
strContent (_:rest) = strContent rest
strContent [] = ""

---- Styling
traverseStyles :: PropertyParser s => (s -> [s] -> s) -> (Txt.Text -> s) -> 
        QueryableStyleSheet s -> XML.Element -> s
traverseStyles = traverseStyles' Nothing temp Nothing
traverseStyles' parent parentStyle previous builder textBuilder stylesheet el@(
        XML.Element _ _ children
    ) = builder style $ traverseChildren Nothing children
    where
        stylishEl = elToStylish el parent previous
        maybeEl = Just stylishEl
        style = cascade stylesheet stylishEl overrides parentStyle
        overrides = [] -- TODO

        traverseChildren prev (XML.NodeContent txt:nodes) =
            textBuilder txt : traverseChildren prev nodes
        traverseChildren prev (XML.NodeElement el:nodes) =
            traverseStyles' maybeEl style prev builder textBuilder stylesheet el :
                traverseChildren (Just $ elToStylish el maybeEl prev) nodes
        traverseChildren prev (_:nodes) = traverseChildren prev nodes
        traverseChildren _ [] = []

elToStylish (XML.Element (XML.Name name _ _) attrs _) parent previous =
    ElementNode {
        name = name,
        attributes = L.sort [
            Attribute (XML.nameLocalName name) (Txt.unpack value)
            | (name, value) <- M.toList attrs
        ],
        parent = parent,
        previous = previous
    }