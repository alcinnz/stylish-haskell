{-# LANGUAGE OverloadedStrings #-}
module Data.HTML2CSS(
        externalStyles, externalStylesForURL, internalStyles, internalStylesForURL, -- legacy
        html2css, cssPriorityAgent, cssPriorityUser, cssPriorityAuthor, -- parsing
        traverseStyles, traversePrepopulatedStyles, traverseStyles', elToStylish -- application
    ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Txt
import Data.Maybe (fromMaybe)

import qualified Text.XML as XML
import Data.CSS.Syntax.StyleSheet
import Data.CSS.Style
import Data.CSS.Syntax.Tokens (tokenize)
import Data.CSS.Preprocessor.Conditions
import qualified Data.CSS.Preprocessor.Conditions.Expr as Query

import Network.URI

---- Constants
cssPriorityAgent, cssPriorityUser, cssPriorityAuthor :: StyleSheet s => s -> s
cssPriorityAgent = setPriority 1
cssPriorityUser = setPriority 2
cssPriorityAuthor = setPriority 3

---- Parsing
html2css :: PropertyParser p => XML.Document -> URI -> ConditionalStyles p
html2css xml url = ConditionalStyles {
    hostURL = url,
    mediaDocument = "document",
    rules = Priority 3 : html2css' (XML.documentRoot xml) (conditionalStyles url "document"),
    propertyParser = temp
}

html2css' :: PropertyParser p => XML.Element -> ConditionalStyles p -> [ConditionalRule p]
html2css' (XML.Element (XML.Name "style" _ _) attrs children) base =
    [Internal (parseMediaQuery attrs) (parseForURL base (hostURL base) $ strContent children)]
html2css' (XML.Element (XML.Name "link" _ _) attrs _) base
    | Just link <- "href" `M.lookup` attrs,
        Just "stylesheet" <- "rel" `M.lookup` attrs,
        Just uri <- parseURIReference $ Txt.unpack link =
            [External (parseMediaQuery attrs) (relativeTo uri $ hostURL base)]
html2css' (XML.Element _ _ children) base = concat [html2css' el base | XML.NodeElement el <- children]

parseMediaQuery :: M.Map XML.Name Txt.Text -> Query.Expr
parseMediaQuery attrs
    | Just text <- "media" `M.lookup` attrs = Query.parse' (tokenize text) []
    | otherwise = []

---- Parsing (legacy)
externalStyles :: StyleSheet s => s -> (M.Map XML.Name Txt.Text -> Bool) ->
        XML.Element -> (URI -> IO Txt.Text) -> IO s
externalStyles a b c d = externalStylesForURL a b c nullURI d
externalStylesForURL stylesheet testMedia html base loadURL = do
    css <- externalStyles' testMedia html base loadURL
    return $ foldl (\a (b, c) -> parseForURL a b c) (cssPriorityAuthor stylesheet) css
externalStyles' testMedia html base loadURL = go $ linkedStyles' testMedia html
    where -- TODO parallelise loads
        go (link:links) = do
            response <- loadURL $ relativeTo link base
            rest <- go links
            return $ (relativeTo link base, response) : rest
        go [] = return []

linkedStyles' testMedia (XML.Element (XML.Name "link" _ _) attrs _)
    | Just link <- "href" `M.lookup` attrs,
        Just "stylesheet" <- "rel" `M.lookup` attrs,
        testMedia attrs,
        Just uri <- parseURIReference $ Txt.unpack link = [uri]
linkedStyles' testMedia (XML.Element _ _ children) =
    concat [linkedStyles' testMedia el | XML.NodeElement el <- children]

internalStyles a b c = internalStylesForURL a b nullURI c
internalStylesForURL testMedia stylesheet base html =
    foldl (\s -> parseForURL s base) (cssPriorityAuthor stylesheet) $
        internalStyles' testMedia html
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
traverseStyles :: PropertyParser s => (s -> [o] -> o) -> (s -> Txt.Text -> o) ->
        QueryableStyleSheet s -> XML.Element -> o
traverseStyles = traverseStyles' Nothing temp Nothing (\x y -> Nothing)
traversePrepopulatedStyles :: PropertyParser s => (s -> XML.Element -> Maybe [o]) ->
        (s -> [o] -> o) -> (s -> Txt.Text -> o) -> QueryableStyleSheet s -> XML.Element -> o
traversePrepopulatedStyles = traverseStyles' Nothing temp Nothing
traverseStyles' :: PropertyParser s => Maybe Element -> s -> Maybe Element ->
        (s -> XML.Element -> Maybe [o]) -> (s -> [o] -> o) -> (s -> Txt.Text -> o) ->
        QueryableStyleSheet s -> XML.Element -> o
traverseStyles' parent parentStyle previous prepopulate builder textBuilder stylesheet el@(
        XML.Element _ attrs children
    ) = builder style traverseChildren
    where
        stylishEl = elToStylish el parent previous
        maybeEl = Just stylishEl
        rules = queryRules stylesheet stylishEl
        style = cascade' (HM.lookupDefault [] "" rules) overrides parentStyle
        overrides | Just styleAttr <- "style" `M.lookup` attrs =
                fst $ parseProperties' $ tokenize styleAttr
            | otherwise = []

        traverseChildren = traversePsuedo' "before" ++
                fromMaybe (traverseChildren' Nothing children) (prepopulate style el) ++
                traversePsuedo' "after"
        traversePsuedo' psuedo = traversePsuedo rules psuedo style builder
        traverseChildren' prev (XML.NodeContent txt:nodes) =
            textBuilder style txt : traverseChildren' prev nodes
        traverseChildren' prev (XML.NodeElement el:nodes) =
            traverseStyles' maybeEl style prev prepopulate builder textBuilder stylesheet el :
                traverseChildren' (Just $ elToStylish el maybeEl prev) nodes
        traverseChildren' prev (_:nodes) = traverseChildren' prev nodes
        traverseChildren' _ [] = []
traversePsuedo rules psuedo parentStyle builder
    | Just rules' <- HM.lookup psuedo rules = [builder (cascade' rules' [] parentStyle) []]
    | otherwise = []

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
addPsuedoclasses el psuedoclasses
    | (Attribute "" value : attrs) <- attributes el = el {
            attributes = Attribute "" (psuedoclasses ++ value) : attrs
        }
    | otherwise = el {
            attributes = Attribute "" psuedoclasses : attributes el
        }
