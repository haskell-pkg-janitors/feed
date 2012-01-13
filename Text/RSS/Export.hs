{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.RSS.Export
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Description: Convert from RSS to XML
--
--------------------------------------------------------------------


module Text.RSS.Export where

import Text.XML as XML
import Text.Feed.Util ()
import Text.RSS.Syntax

import Data.Maybe
import Data.Text (Text, pack)

qualNode :: Text -> [XML.Node] -> XML.Element
qualNode n cs = 
  def 
    { elementName    = qualName n
    , elementNodes = cs
    }

qualName :: Text -> Name
qualName n = Name{nameLocalName=n,nameNamespace=Nothing,namePrefix=Nothing}

---
xmlRSS :: RSS -> XML.Element
xmlRSS r = 
  (qualNode "rss" $ map NodeElement $
    (  [ xmlChannel (rssChannel r) ] 
    ++ rssOther r))
    { elementAttributes = ((qualName "version"), (rssVersion r)):rssAttrs r }

xmlChannel :: RSSChannel -> XML.Element
xmlChannel ch = 
   (qualNode "channel" $ map NodeElement $
     ( [ xmlLeaf "title" (rssTitle ch) 
       , xmlLeaf "link"  (rssLink ch)
       , xmlLeaf "description" (rssDescription ch)
       ]
      ++ map xmlItem (rssItems ch)
      ++ mb (xmlLeaf "language")  (rssLanguage ch)
      ++ mb (xmlLeaf "copyright") (rssCopyright ch)
      ++ mb (xmlLeaf "managingEditor") (rssEditor ch)
      ++ mb (xmlLeaf "webMaster") (rssWebMaster ch)
      ++ mb (xmlLeaf "pubDate")   (rssPubDate ch)
      ++ mb (xmlLeaf "lastBuildDate") (rssLastUpdate ch)
      ++ map xmlCategory (rssCategories ch)
      ++ mb (xmlLeaf "generator") (rssGenerator ch)
      ++ mb (xmlLeaf "docs") (rssDocs ch)
      ++ mb xmlCloud (rssCloud ch)
      ++ mb ((xmlLeaf "ttl") . pack . show) (rssTTL ch)
      ++ mb xmlImage (rssImage ch)
      ++ mb (xmlLeaf "rating") (rssRating ch)
      ++ mb xmlTextInput (rssTextInput ch)
      ++ mb xmlSkipHours (rssSkipHours ch)
      ++ mb xmlSkipDays  (rssSkipDays ch)
      ++ rssChannelOther ch))
      
xmlItem :: RSSItem -> XML.Element
xmlItem it = 
   (qualNode "item" $ map NodeElement $
     (  mb  (xmlLeaf "title") (rssItemTitle it) 
     ++ mb  (xmlLeaf "link")  (rssItemLink it)
     ++ mb  (xmlLeaf "description") (rssItemDescription it)
     ++ mb  (xmlLeaf "author") (rssItemAuthor it)
     ++ map xmlCategory (rssItemCategories it)
     ++ mb  (xmlLeaf "comments") (rssItemComments it)
     ++ mb  xmlEnclosure (rssItemEnclosure it)
     ++ mb  xmlGuid (rssItemGuid it)
     ++ mb  (xmlLeaf "pubDate") (rssItemPubDate it)
     ++ mb  xmlSource (rssItemSource it)
     ++ rssItemOther it))
      { elementAttributes = rssItemAttrs it }

xmlSource :: RSSSource -> XML.Element
xmlSource s = 
   (xmlLeaf "source" (rssSourceTitle s))
     { elementAttributes = ((qualName "url"), (rssSourceURL s)) : 
                   rssSourceAttrs s }

xmlEnclosure :: RSSEnclosure -> XML.Element
xmlEnclosure e = 
   (xmlLeaf "enclosure" "")
     { elementAttributes =
        ((qualName "url"),    (rssEnclosureURL e)) : 
        ((qualName "length"), (pack $ show $ rssEnclosureLength e)) : 
        ((qualName "type"),   (rssEnclosureType e)) : 
	rssEnclosureAttrs e }

xmlCategory :: RSSCategory -> XML.Element
xmlCategory c = 
   (xmlLeaf "category" (rssCategoryValue c))
     { elementAttributes =
        (fromMaybe id (fmap (\ n -> (((qualName "domain"), n):))
	                    (rssCategoryDomain c))) $
	     (rssCategoryAttrs c) }

xmlGuid :: RSSGuid -> XML.Element
xmlGuid g = 
   (xmlLeaf "guid" (rssGuidValue g))
     { elementAttributes =
        (fromMaybe id (fmap (\ n -> (((qualName "isPermaLink"), (toBool n)):))
	                    (rssGuidPermanentURL g))) $
	     (rssGuidAttrs g) }
 where
  toBool False = "false"
  toBool _ = "true"

xmlImage :: RSSImage -> XML.Element
xmlImage im = 
   (qualNode "image" $ map NodeElement $
     ( [ xmlLeaf "url"   (rssImageURL im)
       , xmlLeaf "title" (rssImageTitle im)
       , xmlLeaf "link"  (rssImageLink im)
       ] 
       ++ mb ((xmlLeaf "width")  . pack . show) (rssImageWidth im)
       ++ mb ((xmlLeaf "height") . pack . show) (rssImageHeight im)
       ++ mb (xmlLeaf "description") (rssImageDesc im)
       ++ rssImageOther im))

xmlCloud :: RSSCloud -> XML.Element
xmlCloud cl = 
    (xmlLeaf "cloud" "")
     { elementAttributes =
         (  mb ((,) (qualName "domain")) (rssCloudDomain cl)
	 ++ mb ((,) (qualName "port"))   (rssCloudPort cl)
	 ++ mb ((,) (qualName "path"))   (rssCloudPath cl)
	 ++ mb ((,) (qualName "register")) (rssCloudRegister cl)
	 ++ mb ((,) (qualName "protocol")) (rssCloudProtocol cl)
	 ++ rssCloudAttrs cl) }

xmlTextInput :: RSSTextInput -> XML.Element
xmlTextInput ti =
   (qualNode "textInput" $ map NodeElement $
     ( [ xmlLeaf "title" (rssTextInputTitle ti)
       , xmlLeaf "description"   (rssTextInputDesc ti)
       , xmlLeaf "name"  (rssTextInputName ti)
       , xmlLeaf "link"  (rssTextInputLink ti)
       ] ++ rssTextInputOther ti))
     { elementAttributes = rssTextInputAttrs ti }

xmlSkipHours :: [Integer] -> XML.Element
xmlSkipHours hs = 
  (qualNode "skipHours" $ map NodeElement $
    (map (\ n -> xmlLeaf "hour" (pack $ show n)) hs))

xmlSkipDays :: [Text] -> XML.Element
xmlSkipDays hs = 
  (qualNode "skipDayss" $ map NodeElement $
    (map (\ n -> xmlLeaf "day" n) hs))

--

xmlAttr :: Text -> Text -> (XML.Name,Text)
xmlAttr k v = ((qualName k), v)

xmlLeaf :: Text -> Text -> XML.Element
xmlLeaf tg txt = 
 def{ elementName = qualName tg
 	      , elementNodes = [ NodeContent txt ]
	      }

---
mb :: (a -> b) -> Maybe a -> [b]
mb _ Nothing = []
mb f (Just x) = [f x]

