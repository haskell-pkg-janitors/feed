{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.RSS1.Utils
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

module Text.RSS1.Utils where

import Text.XML as XML
import Text.Feed.Util as XML
import Text.DublinCore.Types

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)

pQNodes :: Name -> XML.Element -> [XML.Element]
pQNodes = XML.findChildren

pNode      :: Text -> XML.Element -> Maybe XML.Element
pNode x e  = listToMaybe (pQNodes (qualName (rss10NS,Nothing) x) e)

pQNode        :: Name -> XML.Element -> Maybe XML.Element
pQNode x e    = listToMaybe (pQNodes x e)

pLeaf        :: Text -> XML.Element -> Maybe Text
pLeaf x e    = strContent `fmap` pQNode (qualName (rss10NS,Nothing) x) e

pQLeaf        :: (Maybe Text,Maybe Text) -> Text -> XML.Element -> Maybe Text
pQLeaf ns x e = strContent `fmap` pQNode (qualName ns x) e

pAttr        :: (Maybe Text, Maybe Text) -> Text -> XML.Element -> Maybe Text
pAttr ns x e = lookup (qualName ns x) [ (k,v) | (k,v) <- elementAttributes e ]

pMany        :: (Maybe Text,Maybe Text) -> Text -> (XML.Element -> Maybe a) -> XML.Element -> [a]
pMany ns p f e  = mapMaybe f (pQNodes (qualName ns p) e)

children     :: XML.Element -> [XML.Element]
children e    = onlyElems (elementNodes e)

qualName :: (Maybe Text, Maybe Text) -> Text -> Name
qualName (ns,pre) x = Name{nameLocalName=x,nameNamespace=ns,namePrefix=pre}

rssPrefix, rss10NS :: Maybe Text
rss10NS = Just "http://purl.org/rss/1.0/"
rssPrefix = Nothing

rdfPrefix, rdfNS :: Maybe Text
rdfNS = Just "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdfPrefix = Just "rdf"

synPrefix, synNS :: Maybe Text
synNS = Just "http://purl.org/rss/1.0/modules/syndication/"
synPrefix = Just "sy"

taxPrefix, taxNS :: Maybe Text
taxNS = Just "http://purl.org/rss/1.0/modules/taxonomy/"
taxPrefix = Just "taxo"

conPrefix, conNS :: Maybe Text
conNS = Just "http://purl.org/rss/1.0/modules/content/"
conPrefix = Just "content"

dcPrefix, dcNS :: Maybe Text
dcNS = Just "http://purl.org/dc/elements/1.1/"
dcPrefix = Just "dc"

rdfName :: Text -> Name
rdfName x = Name{nameLocalName=x,nameNamespace=rdfNS,namePrefix=rdfPrefix}

rssName :: Text -> Name
rssName x = Name{nameLocalName=x,nameNamespace=rss10NS,namePrefix=rssPrefix}

synName :: Text -> Name
synName x = Name{nameLocalName=x,nameNamespace=synNS,namePrefix=synPrefix}

known_rss_elts :: [Name]
known_rss_elts = map rssName [ "channel", "item", "image", "textinput" ]

known_syn_elts :: [Name]
known_syn_elts = map synName [ "updateBase", "updateFrequency", "updatePeriod" ]

known_dc_elts :: [Name]
known_dc_elts  = map (qualName (dcNS,dcPrefix)) dc_element_names

known_tax_elts :: [Name]
known_tax_elts = map (qualName (taxNS,taxPrefix)) [ "topic", "topics" ]

known_con_elts :: [Name]
known_con_elts = map (qualName (conNS,conPrefix)) [ "items", "item", "format", "encoding" ]

removeKnownElts :: XML.Element -> [XML.Element]
removeKnownElts e = 
  filter (\ e1 -> not (elementName e1 `elem` known_elts)) (children e)
 where
  known_elts = 
    concat [ known_rss_elts 
           , known_syn_elts
	   , known_dc_elts
	   , known_con_elts
	   , known_tax_elts
	   ]

removeKnownAttrs :: XML.Element -> [(XML.Name,Text)]
removeKnownAttrs e = 
  filter (\ (k,_) -> not (k `elem` known_attrs)) (elementAttributes e)
 where
  known_attrs = 
     map rdfName [ "about" ]

