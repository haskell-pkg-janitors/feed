{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.RSS1.Export
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

module Text.RSS1.Export
       ( xmlFeed
       ) where

import Text.XML as XML
import Text.RSS1.Syntax
import Text.RSS1.Utils
import Text.DublinCore.Types

import Data.List
import Data.Maybe
import Data.Text (Text, pack)

qualNode :: (Maybe Text,Maybe Text) -> Text -> [XML.Node] -> XML.Element
qualNode ns n cs = 
  def
    { elementName    = qualName ns n
    , elementNodes = cs
    }

---
xmlFeed :: Feed -> XML.Element
xmlFeed f = 
  (qualNode (rdfNS,rdfPrefix) "RDF" $ map NodeElement $
    (concat  [ [xmlChannel (feedChannel f)]
             , mb xmlImage (feedImage f)
             , map xmlItem (feedItems f)
             , mb xmlTextInput (feedTextInput f)
             , map xmlTopic (feedTopics f)
             , feedOther f
             ] ))
        -- should we expect these to be derived by the XML pretty printer..?
    { elementAttributes =   nub $
                    ((qualName  (Nothing,Nothing) "xmlns"), (fromJust rss10NS))
                    ((qualName (Nothing,Just "xmlns") (fromJust rdfPrefix)), (fromJust rdfNS)) :
                    ((qualName (Nothing,Just "xmlns") (fromJust synPrefix)), (fromJust synNS)) :
                    ((qualName (Nothing,Just "xmlns") (fromJust taxPrefix)), (fromJust taxNS)) :
                    ((qualName (Nothing,Just "xmlns") (fromJust conPrefix)), (fromJust conNS)) :
                    ((qualName (Nothing,Just "xmlns") (fromJust dcPrefix)),  (fromJust dcNS))  :
                    feedAttrs f}

xmlChannel :: Channel -> XML.Element
xmlChannel ch = 
  (qualNode (rss10NS,Nothing) "channel" $ map NodeElement $
     ([ xmlLeaf  (rss10NS,Nothing) "title" (channelTitle ch)
      , xmlLeaf  (rss10NS,Nothing) "link"  (channelLink ch)
      , xmlLeaf  (rss10NS,Nothing) "description" (channelDesc ch)
      ] ++ 
      mb xmlTextInputURI (channelTextInputURI ch) ++ 
      mb xmlImageURI (channelImageURI ch) ++ 
      xmlItemURIs (channelItemURIs ch) ++ map xmlDC (channelDC ch) ++
      concat [ mb xmlUpdatePeriod (channelUpdatePeriod ch)
             , mb xmlUpdateFreq   (channelUpdateFreq ch)
             , mb (xmlLeaf (synNS,synPrefix) "updateBase")   (channelUpdateBase ch)
             ] ++ 
      xmlContentItems (channelementNodes ch) ++
      xmlTopics       (channelTopics ch) ++
      channelOther ch))
    { elementAttributes = ( ((qualName  (rdfNS,rdfPrefix) "about"), (channelURI ch)) :
                    channelAttrs ch)}

xmlImageURI :: URIString -> XML.Element
xmlImageURI u = xmlEmpty (rss10NS,Nothing) "image" [((rdfName "resource"), u) ]

xmlImage :: Image -> XML.Element
xmlImage i = 
 (qualNode (rss10NS,Nothing) "image" $ map NodeElement $
    ([ xmlLeaf  (rss10NS,Nothing) "title" (imageTitle i)
     ,  xmlLeaf (rss10NS,Nothing) "url"   (imageURL i)
     , xmlLeaf  (rss10NS,Nothing) "link"  (imageLink i)
     ] ++ map xmlDC (imageDC i) ++
     imageOther i))
    { elementAttributes = ( ((qualName  (rdfNS,rdfPrefix) "about"), (imageURI i)) :
                    imageAttrs i)}

xmlItemURIs :: [URIString] -> [XML.Element]
xmlItemURIs [] = []
xmlItemURIs xs = 
  [qualNode (rss10NS, Nothing) "items" $ 
      [NodeElement (qualNode (rdfNS,rdfPrefix) "Seq" (map toRes xs))]]
 where
  toRes u = NodeElement (xmlEmpty (rdfNS,rdfPrefix) "li" [((rdfName "resource"), u)])

xmlTextInputURI :: URIString -> XML.Element
xmlTextInputURI u = xmlEmpty (rss10NS,Nothing) "textinput" [((rdfName "resource"), u) ]

xmlTextInput :: TextInputInfo -> XML.Element
xmlTextInput ti = 
  (qualNode (rss10NS, Nothing) "textinput" $ map NodeElement $
     [ xmlLeaf (rss10NS,Nothing) "title" (textInputTitle ti)
     , xmlLeaf (rss10NS,Nothing) "description" (textInputDesc ti)
     , xmlLeaf (rss10NS,Nothing) "name" (textInputName ti)
     , xmlLeaf (rss10NS,Nothing) "link" (textInputLink ti)
     ] ++ map xmlDC (textInputDC ti) ++
     textInputOther ti)
     {elementAttributes=((rdfName "about"), (textInputURI ti)) : textInputAttrs ti}

xmlDC :: DCItem -> XML.Element
xmlDC dc = xmlLeaf (dcNS,dcPrefix) (infoToTag (dcElt dc)) (dcText dc)

xmlUpdatePeriod :: UpdatePeriod -> XML.Element
xmlUpdatePeriod u = xmlLeaf (synNS,synPrefix) "updatePeriod" (toStr u)
 where
  toStr ux = 
    case ux of
      Update_Hourly  -> "hourly"
      Update_Daily   -> "daily"
      Update_Weekly  -> "weekly"
      Update_Monthly -> "monthly"
      Update_Yearly  -> "yearly"

xmlUpdateFreq :: Integer -> XML.Element
xmlUpdateFreq f = xmlLeaf (synNS,synPrefix) "updateFrequency" (pack $ show f)

xmlContentItems :: [ContentInfo] -> [XML.Element]
xmlContentItems [] = []
xmlContentItems xs = 
  [qualNode (conNS,conPrefix) "items" 
    [NodeElement $ qualNode (rdfNS,rdfPrefix) "Bag"
              (map (\ e -> NodeElement (qualNode (rdfNS,rdfPrefix) "li" [NodeElement (xmlContentInfo e)])) 
                   xs)]]

xmlContentInfo :: ContentInfo -> XML.Element
xmlContentInfo ci = 
  (qualNode (conNS,conPrefix) "item" $ map NodeElement $
      (concat [ mb (rdfResource (conNS,conPrefix) "format") (contentFormat ci)
              , mb (rdfResource (conNS,conPrefix) "encoding") (contentEncoding ci)
              , mb (rdfValue []) (contentValue ci)
              ]))
    {elementAttributes=mb ((,) (rdfName "about")) (contentURI ci)}

rdfResource :: (Maybe Text,Maybe Text) -> Text -> Text -> XML.Element
rdfResource ns t v = xmlEmpty ns t [((rdfName "resource"), v) ]

rdfValue :: [(XML.Name,Text)] -> Text -> XML.Element
rdfValue as s = (xmlLeaf (rdfNS,rdfPrefix) "value" s){elementAttributes=as}

xmlTopics :: [URIString] -> [XML.Element]
xmlTopics [] = []
xmlTopics xs = 
 [qualNode (taxNS,taxPrefix) "topics"
    [NodeElement (qualNode (rdfNS,rdfPrefix) "Bag" $
            (map (NodeElement . rdfResource (rdfNS,rdfPrefix) "li") xs))]]

xmlTopic :: TaxonomyTopic -> XML.Element
xmlTopic tt = 
  (qualNode (taxNS,taxPrefix) "topic" $ map NodeElement $
      (xmlLeaf (rss10NS,Nothing) "link"  (taxonomyLink tt):
        mb (xmlLeaf (rss10NS,Nothing) "title") (taxonomyTitle tt) ++
       mb (xmlLeaf (rss10NS,Nothing) "description") (taxonomyDesc tt) ++
       xmlTopics (taxonomyTopics tt) ++
       map xmlDC (taxonomyDC tt) ++
       taxonomyOther tt))
    {elementAttributes=[((rdfName "about"), (taxonomyURI tt))]}

xmlItem :: Item -> XML.Element
xmlItem i = 
 (qualNode (rss10NS,Nothing) "item" $ map NodeElement $
    ([ xmlLeaf  (rss10NS,Nothing) "title" (itemTitle i)
     , xmlLeaf  (rss10NS,Nothing) "link"  (itemLink i)
     ] ++ 
     mb (xmlLeaf  (rss10NS,Nothing) "description") (itemDesc i) ++
     map xmlDC (itemDC i) ++
     xmlTopics (itemTopics i) ++
     map xmlContentInfo (itemContent i) ++
     itemOther i))
    { elementAttributes = ( ((qualName  (rdfNS,rdfPrefix) "about"), (itemURI i)) :
                    itemAttrs i)}

xmlLeaf :: (Maybe Text,Maybe Text) -> Text -> Text -> XML.Element
xmlLeaf ns tg txt = 
 def{ elementName = qualName ns tg
              , elementNodes = [ NodeContent txt ]
              }

xmlEmpty :: (Maybe Text,Maybe Text) -> Text -> [(XML.Name,Text)] -> XML.Element
xmlEmpty ns t as = (qualNode ns t []){elementAttributes=as}

---
mb :: (a -> b) -> Maybe a -> [b]
mb _ Nothing = []
mb f (Just x) = [f x]
