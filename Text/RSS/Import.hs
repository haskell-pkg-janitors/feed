{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.RSS.Import
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Description: Converting from XML to RSS
--
--------------------------------------------------------------------

module Text.RSS.Import where

import Text.Feed.Util
import Text.RSS.Syntax
import Text.RSS1.Utils ( dcNS, dcPrefix )
import Text.XML as XML

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text as T (Text, unpack)
import Data.Char  (isSpace )
import Control.Monad (guard,mplus)

pNodes       :: Text -> [XML.Element] -> [XML.Element]
pNodes x es   = filter ((qualName x ==) . elementName) es

pQNodes       :: Name -> [XML.Element] -> [XML.Element]
pQNodes x es   = filter ((x==) . elementName) es

pNode        :: Text -> [XML.Element] -> Maybe XML.Element
pNode x es    = listToMaybe (pNodes x es)

pQNode        :: Name -> [XML.Element] -> Maybe XML.Element
pQNode x es    = listToMaybe (pQNodes x es)

pLeaf        :: Text -> [XML.Element] -> Maybe Text
pLeaf x es    = strContent `fmap` pNode x es

pQLeaf      :: Name -> [XML.Element] -> Maybe Text
pQLeaf x es  = strContent `fmap` (pQNode x es)

pAttr        :: Text -> XML.Element -> Maybe Text
pAttr x e     = lookup (qualName x) [ (k,v) | (k, v) <- elementAttributes e ]

pMany        :: Text -> (XML.Element -> Maybe a) -> [XML.Element] -> [a]
pMany p f es  = mapMaybe f (pNodes p es)

children     :: XML.Element -> [XML.Element]
children e    = onlyElems (elementNodes e)

qualName :: Text -> Name
qualName x = Name{nameLocalName=x,nameNamespace=Nothing,namePrefix=Nothing}

dcName :: Text -> Name
dcName x = def{nameLocalName=x,nameNamespace=dcNS,namePrefix=dcPrefix}

elementToRSS :: XML.Element -> Maybe RSS
elementToRSS e = do
  guard (elementName e == qualName "rss")
  let es = children e
  let as = elementAttributes e
  v  <- pAttr "version" e
  ch <- pNode "channel" es >>= elementToChannel 
  return RSS
    { rssVersion = v
    , rssAttrs   = filter (\ (k,_) -> not (nameLocalName (k) `elem` known_attrs)) as
    , rssChannel = ch
    , rssOther   = filter (\ e1 -> elementName e1 /= qualName "channel") es
    }
 where
  known_attrs = ["version"]

elementToChannel :: XML.Element -> Maybe RSSChannel
elementToChannel e = do
  guard (elementName e == qualName "channel")
  let es = children e
  title <- pLeaf "title" es
  link  <- pLeaf "link"  es
  desc  <- pLeaf "description" es
  return RSSChannel
     { rssTitle = title
     , rssLink  = link
     , rssDescription = desc
     , rssItems = pMany "item" elementToItem es
     , rssLanguage   = pLeaf "language" es `mplus` pQLeaf (dcName "lang") es
     , rssCopyright  = pLeaf "copyright" es
     , rssEditor     = pLeaf "managingEditor" es `mplus` pQLeaf (dcName "creator") es
     , rssWebMaster  = pLeaf "webMaster" es
     , rssPubDate    = pLeaf "pubDate" es `mplus` pQLeaf (dcName "date") es
     , rssLastUpdate = pLeaf "lastBuildDate" es `mplus` pQLeaf (dcName "date") es
     , rssCategories = pMany "category"  elementToCategory es
     , rssGenerator  = pLeaf "generator" es `mplus` pQLeaf (dcName "source") es
     , rssDocs       = pLeaf "docs" es
     , rssCloud      = pNode "cloud" es >>= elementToCloud
     , rssTTL        = pLeaf "ttl" es   >>= readInt
     , rssImage      = pNode "image" es >>= elementToImage
     , rssRating     = pLeaf "rating" es
     , rssTextInput  = pNode "textInput" es >>= elementToTextInput
     , rssSkipHours  = pNode "skipHours" es >>= elementToSkipHours
     , rssSkipDays   = pNode "skipDays" es  >>= elementToSkipDays
     , rssChannelOther = filter (\ e1 -> not (elementName e1 `elem` known_channel_elts)) es
     }
 where
  known_channel_elts = map qualName
     [ "title", "link", "description"
     , "item", "language", "copyright"
     , "managingEditor", "webMaster"
     , "pubDate", "lastBuildDate"
     , "category", "generator", "docs"
     , "cloud", "ttl", "image"
     , "rating", "textInput"
     , "skipHours", "skipDays"
     ]

elementToImage :: XML.Element -> Maybe RSSImage
elementToImage e = do
  guard (elementName e == qualName "image")
  let es = children e
  url   <- pLeaf "url"  es
  title <- pLeaf "title" es
  link  <- pLeaf "link" es
  return RSSImage
    { rssImageURL    = url
    , rssImageTitle  = title
    , rssImageLink   = link
    , rssImageWidth  = pLeaf "width" es  >>= readInt
    , rssImageHeight = pLeaf "height" es >>= readInt
    , rssImageDesc   = pLeaf "description" es
    , rssImageOther  = filter (\ e1 -> not (elementName e1 `elem` known_image_elts)) es
    }
 where
   known_image_elts = map qualName
      [ "url", "title", "link"
      , "width", "height", "description"
      ]

elementToCategory :: XML.Element -> Maybe RSSCategory
elementToCategory e = do
  guard (elementName e == qualName "category")
  let as = elementAttributes e
  return RSSCategory
    { rssCategoryDomain = pAttr "domain" e
    , rssCategoryAttrs  = filter (\ (k,_) -> not (nameLocalName k `elem` known_attrs)) as
    , rssCategoryValue  = strContent e
    }
 where
  known_attrs = ["domain"]

elementToCloud :: XML.Element -> Maybe RSSCloud
elementToCloud e = do
  guard (elementName e == qualName "cloud")
  let as = elementAttributes e
  return RSSCloud
    { rssCloudDomain   = pAttr "domain" e
    , rssCloudPort     = pAttr "port" e
    , rssCloudPath     = pAttr "path" e
    , rssCloudRegister = pAttr "register" e
    , rssCloudProtocol = pAttr "protocol" e
    , rssCloudAttrs    = filter (\ (k,_) -> not (nameLocalName k `elem` known_attrs)) as
    }
 where
  known_attrs = [ "domain", "port", "path", "register", "protocol" ]

elementToItem :: XML.Element -> Maybe RSSItem
elementToItem e = do
  guard (elementName e == qualName "item")
  let es = children e
  return RSSItem
    { rssItemTitle       = pLeaf "title" es
    , rssItemLink        = pLeaf "link" es
    , rssItemDescription = pLeaf "description" es
    , rssItemAuthor      = pLeaf "author" es `mplus` pQLeaf (dcName "creator") es
    , rssItemCategories  = pMany "category" elementToCategory es
    , rssItemComments    = pLeaf "comments" es
    , rssItemEnclosure   = pNode "enclosure" es >>= elementToEnclosure
    , rssItemGuid        = pNode "guid" es      >>= elementToGuid
    , rssItemPubDate     = pLeaf "pubDate" es `mplus` pQLeaf (dcName "date") es
    , rssItemSource      = pNode "source" es    >>= elementToSource
    , rssItemAttrs       = elementAttributes e
    , rssItemOther       = filter (\ e1 -> not (elementName e1 `elem` known_item_elts)) es
    }
 where
  known_item_elts = map qualName
    [ "title", "link", "description"
    , "author", "category", "comments"
    , "enclosure", "guid", "pubDate"
    , "source"
    ]

elementToSource :: XML.Element -> Maybe RSSSource
elementToSource e = do
  guard (elementName e == qualName "source")
  let as = elementAttributes e
  url <- pAttr "url" e
  return RSSSource
    { rssSourceURL = url
    , rssSourceAttrs = filter (\ (k,_) -> not (nameLocalName k `elem` known_attrs)) as
    , rssSourceTitle = strContent e
    }
 where
  known_attrs = [ "url" ]

elementToEnclosure :: XML.Element -> Maybe RSSEnclosure
elementToEnclosure e = do
  guard (elementName e == qualName "enclosure")
  let as = elementAttributes e
  url <- pAttr "url" e
  ty  <- pAttr "type" e
  len <- pAttr "length" e >>= readInt
  return RSSEnclosure
    { rssEnclosureURL = url
    , rssEnclosureType = ty
    , rssEnclosureLength = len
    , rssEnclosureAttrs = filter (\ (k,_) -> not (nameLocalName k `elem` known_attrs)) as
    }
 where
  known_attrs = [ "url", "type", "length" ]

elementToGuid :: XML.Element -> Maybe RSSGuid
elementToGuid e = do
  guard (elementName e == qualName "guid")
  let as = elementAttributes e
  return RSSGuid
    { rssGuidPermanentURL = pAttr "isPermaLink" e >>= readBool
    , rssGuidAttrs        = filter (\ (k,_) -> not (nameLocalName k `elem` known_attrs)) as
    , rssGuidValue        = strContent e
    }
 where
  known_attrs = ["isPermaLink"]

elementToTextInput :: XML.Element -> Maybe RSSTextInput
elementToTextInput e = do
  guard (elementName e == qualName "textInput")
  let es = children e
  title <- pLeaf "title" es
  desc  <- pLeaf "description" es
  name  <- pLeaf "name" es
  link  <- pLeaf "link" es
  return RSSTextInput
    { rssTextInputTitle = title
    , rssTextInputDesc  = desc
    , rssTextInputName  = name
    , rssTextInputLink  = link
    , rssTextInputAttrs = elementAttributes e
    , rssTextInputOther = filter (\ e1 -> not (elementName e1 `elem` known_ti_elts)) es
    }
 where
  known_ti_elts = map qualName
    ["title", "description", "name", "link"]

elementToSkipHours :: XML.Element -> Maybe [Integer]
elementToSkipHours e = do
  guard (elementName e == qualName "skipHours")
     -- don't bother checking that this is below limit ( <= 24)
  return (pMany "hour" (readInt.strContent) (children e))

elementToSkipDays :: XML.Element -> Maybe [Text]
elementToSkipDays e = do
  guard (elementName e == qualName "skipDays")
     -- don't bother checking that this is below limit ( <= 7)
  return (pMany "day" (return . strContent) (children e))

----

readInt :: Text -> Maybe Integer
readInt s = 
  case reads $ unpack s of
    ((x,_):_) -> Just x
    _ -> Nothing

readBool :: Text -> Maybe Bool
readBool s = 
  case dropWhile isSpace $ unpack s of
    't':'r':'u':'e':_ -> Just True
    'f':'a':'l':'s':'e':_ -> Just False
    _ -> Nothing

