{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.Atom.Pub.Export
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Description: Serializing APP types (as XML.)
--
--------------------------------------------------------------------
module Text.Atom.Pub.Export where

import Text.XML
import Text.Feed.Util
import Text.Atom.Pub
import Text.Atom.Feed.Export 
       ( mb, xmlCategory, xmlTitle
       , xmlns_atom
       )

import Data.Text (Text)

showServiceDoc :: Service -> Text
showServiceDoc s = showElement (xmlService s)

-- ToDo: old crud; inline away.
mkName :: Maybe Text -> Text -> Name
mkName a b = def{namePrefix=a,nameLocalName=b}

mkElem :: Name -> [Attr] -> [Element] -> Element
mkElem a b c = Element {elementName=a 
                       ,elementAttributes=b
                       ,elementNodes=map NodeElement c
                       }

mkLeaf :: Name -> [Attr] -> Text -> Element
mkLeaf a b c = Element {elementName=a, elementAttributes=b, elementNodes=[NodeContent c]}

mkAttr :: Text -> Text -> Attr
mkAttr a b  = (mkName Nothing a,b)

xmlns_app :: Attr
xmlns_app = ((mkName (Just "xmlns") "app"), appNS)


appNS :: Text
appNS = "http://purl.org/atom/app#"

appName :: Text -> Name
appName nc = (mkName (Just "app") nc){nameNamespace=Just appNS}

xmlService :: Service -> Element
xmlService s = 
  mkElem (appName "service") [xmlns_app,xmlns_atom]
         (concat [ map xmlWorkspace (serviceWorkspaces s)
	         , serviceOther s
		 ])

xmlWorkspace :: Workspace -> Element
xmlWorkspace w = 
  mkElem (appName "workspace") 
         [mkAttr "xml:lang" "en"]
	 (concat [ [xmlTitle (workspaceTitle w)]
	         , map xmlCollection (workspaceCols w)
		 , workspaceOther w
		 ])

xmlCollection :: Collection -> Element
xmlCollection c =
  mkElem (appName "collection")
         [mkAttr "href" (collectionURI c)]
	 (concat [ [xmlTitle (collectionTitle c)]
	         , map xmlAccept (collectionAccept c)
		 , map xmlCategories (collectionCats c)
		 , collectionOther c
		 ])
		 
xmlCategories :: Categories -> Element
xmlCategories (CategoriesExternal u) = 
  mkElem (appName "categories") [mkAttr "href" u] []
xmlCategories (Categories mbFixed mbScheme cs) = 
  mkElem (appName "categories")
         (concat [ mb (\ f -> mkAttr "fixed"  (if f then "yes" else "no")) mbFixed
	         , mb (mkAttr "scheme") mbScheme
		 ])
	 (map xmlCategory cs)

xmlAccept :: Accept -> Element
xmlAccept a = mkLeaf (appName "accept") [] (acceptType a)
