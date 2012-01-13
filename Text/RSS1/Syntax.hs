{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.RSS1.Syntax
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

module Text.RSS1.Syntax where

import Data.Text
import Text.XML as XML
import Text.DublinCore.Types

type URIString   = Text
type TitleString = Text
type TimeString  = Text
type TextText  = Text

data Feed
 = Feed { feedVersion   :: Text
        , feedChannel   :: Channel
        , feedImage     :: Maybe Image
        , feedItems     :: [Item]
        , feedTextInput :: Maybe TextInputInfo
        , feedTopics    :: [TaxonomyTopic]
        , feedOther     :: [XML.Element]
        , feedAttrs     :: [(XML.Name,Text)]
        }
        deriving (Show)

data Channel
 = Channel
        { channelURI          :: URIString
        , channelTitle        :: TitleString
        , channelLink         :: URIString
        , channelDesc         :: TextText
           -- these are indirect RDF associations to elements declared
           -- outside the channel element in the RDF \/ feed document.
        , channelImageURI     :: Maybe URIString
        , channelItemURIs     :: [URIString]
        , channelTextInputURI :: Maybe URIString
        , channelDC           :: [DCItem]
        , channelUpdatePeriod :: Maybe UpdatePeriod
        , channelUpdateFreq   :: Maybe Integer
        , channelUpdateBase   :: Maybe TimeString   -- format is yyyy-mm-ddThh:mm
        , channelementNodes      :: [ContentInfo]
        , channelTopics       :: [URIString]
        , channelOther        :: [XML.Element]
        , channelAttrs        :: [(XML.Name,Text)]
        }
        deriving (Show)

data Image
 = Image
        { imageURI    :: URIString   -- the image resource, most likely.
        , imageTitle  :: TextText  -- the "alt"ernative text.
        , imageURL    :: URIString
        , imageLink   :: URIString   -- the href of the rendered img resource.
        , imageDC     :: [DCItem]
        , imageOther  :: [XML.Element]
        , imageAttrs  :: [(XML.Name,Text)]
        }
        deriving (Show)

data Item
 = Item
        { itemURI     :: URIString
        , itemTitle   :: TextText
        , itemLink    :: URIString
        , itemDesc    :: Maybe TextText
        , itemDC      :: [DCItem]
        , itemTopics  :: [URIString]
        , itemContent :: [ContentInfo]
        , itemOther   :: [XML.Element]
        , itemAttrs   :: [(XML.Name,Text)]
        }
        deriving (Show)

data TextInputInfo
 = TextInputInfo
        { textInputURI   :: URIString
        , textInputTitle :: TextText
        , textInputDesc  :: TextText
        , textInputName  :: TextText
        , textInputLink  :: URIString
        , textInputDC    :: [DCItem]
        , textInputOther :: [XML.Element]
        , textInputAttrs :: [(XML.Name,Text)]
        }
        deriving (Show)

data TaxonomyTopic
 = TaxonomyTopic
        { taxonomyURI    :: URIString
        , taxonomyLink   :: URIString
        , taxonomyTitle  :: Maybe Text
        , taxonomyDesc   :: Maybe Text
        , taxonomyTopics :: [URIString]
        , taxonomyDC     :: [DCItem]
        , taxonomyOther  :: [XML.Element]
        }
        deriving (Show)


data UpdatePeriod 
 = Update_Hourly
 | Update_Daily
 | Update_Weekly
 | Update_Monthly
 | Update_Yearly
        deriving (Eq, Show)

data ContentInfo
 = ContentInfo
        { contentURI      :: Maybe URIString
        , contentFormat   :: Maybe URIString
        , contentEncoding :: Maybe URIString
        , contentValue    :: Maybe Text -- should be: RDFValue
        }
        deriving (Eq, Show)

--default constructors:
nullFeed :: URIString -> TitleString -> Feed
nullFeed uri title = 
   Feed { feedVersion   = "1.0"
        , feedChannel   = nullChannel uri title
        , feedImage     = Nothing
        , feedItems     = []
        , feedTextInput = Nothing
        , feedTopics    = []
        , feedOther     = []
        , feedAttrs     = []
        }

nullChannel :: URIString -> TitleString -> Channel
nullChannel uri title = 
   Channel
        { channelURI          = uri
        , channelTitle        = title
        , channelLink         = uri
        , channelDesc         = title
        , channelImageURI     = Nothing
        , channelItemURIs     = []
        , channelTextInputURI = Nothing
        , channelDC           = []
        , channelUpdatePeriod = Nothing
        , channelUpdateFreq   = Nothing
        , channelUpdateBase   = Nothing
        , channelementNodes      = []
        , channelTopics       = []
        , channelOther        = []
        , channelAttrs        = []
        }

nullImage :: URIString -> Text -> URIString -> Image
nullImage imguri title link = 
  Image
        { imageURI    = imguri
        , imageTitle  = title
        , imageURL    = imguri
        , imageLink   = link
        , imageDC     = []
        , imageOther  = []
        , imageAttrs  = []
        }

nullItem :: URIString -> TextText -> URIString -> Item
nullItem uri title link = 
  Item
        { itemURI     = uri
        , itemTitle   = title
        , itemLink    = link
        , itemDesc    = Nothing
        , itemDC      = []
        , itemTopics  = []
        , itemContent = []
        , itemOther   = []
        , itemAttrs   = []
        }

nullTextInputInfo :: URIString -> TextText -> TextText -> URIString -> TextInputInfo
nullTextInputInfo uri title nm link =
  TextInputInfo
        { textInputURI   = uri
        , textInputTitle = title
        , textInputDesc  = title
        , textInputName  = nm
        , textInputLink  = link
        , textInputDC    = []
        , textInputOther = []
        , textInputAttrs = []
        }

nullTaxonomyTopic :: URIString -> URIString -> TaxonomyTopic
nullTaxonomyTopic uri link = 
  TaxonomyTopic
        { taxonomyURI    = uri
        , taxonomyLink   = link
        , taxonomyTitle  = Nothing
        , taxonomyDesc   = Nothing
        , taxonomyTopics = []
        , taxonomyDC     = []
        , taxonomyOther  = []
        }

nullContentInfo :: ContentInfo
nullContentInfo =
  ContentInfo
        { contentURI      = Nothing
        , contentFormat   = Nothing
        , contentEncoding = Nothing
        , contentValue    = Nothing
        }

