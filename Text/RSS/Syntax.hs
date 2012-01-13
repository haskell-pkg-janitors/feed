{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.RSS.Syntax
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Description: The basic syntax for putting together feeds.  For instance,
-- to create a feed with a single item item:
--  (nullRSS \"rss title\" \"link\") {rssChannel=(nullChannel \"channel title\" \"link\") {rssItems=[(nullItem \"item title\")]}}

--------------------------------------------------------------------


module Text.RSS.Syntax where

import Data.Text
import Text.XML as XML

-- * Core Types

-- ^The Radio Userland version of RSS documents\/feeds.
-- (versions 0.9x, 2.x)
data RSS
 = RSS
     { rssVersion :: Text
     , rssAttrs   :: [(XML.Name,Text)]
     , rssChannel :: RSSChannel
     , rssOther   :: [XML.Element]
     }
     deriving (Show)

type URLText  = Text
-- | RFC 822 conforming.
type DateText = Text 

data RSSChannel
 = RSSChannel
     { rssTitle        :: Text
     , rssLink         :: URLText
     , rssDescription  :: Text
     , rssItems        :: [RSSItem]
     , rssLanguage     :: Maybe Text
     , rssCopyright    :: Maybe Text
     , rssEditor       :: Maybe Text
     , rssWebMaster    :: Maybe Text
     , rssPubDate      :: Maybe DateText  -- ^ rfc 822 conforming.
     , rssLastUpdate   :: Maybe DateText  -- ^ rfc 822 conforming.
     , rssCategories   :: [RSSCategory]
     , rssGenerator    :: Maybe Text
     , rssDocs         :: Maybe URLText
     , rssCloud        :: Maybe RSSCloud
     , rssTTL          :: Maybe Integer
     , rssImage        :: Maybe RSSImage
     , rssRating       :: Maybe Text
     , rssTextInput    :: Maybe RSSTextInput
     , rssSkipHours    :: Maybe [Integer]
     , rssSkipDays     :: Maybe [Text]
     , rssChannelOther :: [XML.Element]
     }
     deriving (Show)

data RSSItem
 = RSSItem
     { rssItemTitle        :: Maybe Text
     , rssItemLink         :: Maybe URLText
     , rssItemDescription  :: Maybe Text     -- ^if not present, the title is. (per spec, at least.)
     , rssItemAuthor       :: Maybe Text
     , rssItemCategories   :: [RSSCategory]
     , rssItemComments     :: Maybe URLText
     , rssItemEnclosure    :: Maybe RSSEnclosure
     , rssItemGuid         :: Maybe RSSGuid
     , rssItemPubDate      :: Maybe DateText
     , rssItemSource       :: Maybe RSSSource
     , rssItemAttrs        :: [(XML.Name,Text)]
     , rssItemOther        :: [XML.Element]
     }
     deriving (Show)

data RSSSource
 = RSSSource
     { rssSourceURL    :: URLText
     , rssSourceAttrs  :: [(XML.Name,Text)]
     , rssSourceTitle  :: Text
     }
     deriving (Show)

data RSSEnclosure
 = RSSEnclosure
     { rssEnclosureURL     :: URLText
     , rssEnclosureLength  :: Integer
     , rssEnclosureType    :: Text
     , rssEnclosureAttrs   :: [(XML.Name,Text)]
     }
     deriving (Show)

data RSSCategory
 = RSSCategory
     { rssCategoryDomain   :: Maybe Text
     , rssCategoryAttrs    :: [(XML.Name,Text)]
     , rssCategoryValue    :: Text
     }
     deriving (Show)

data RSSGuid
 = RSSGuid
     { rssGuidPermanentURL :: Maybe Bool
     , rssGuidAttrs        :: [(XML.Name,Text)]
     , rssGuidValue        :: Text
     }
     deriving (Show)


data RSSImage
 = RSSImage
     { rssImageURL     :: URLText -- the URL to the image resource.
     , rssImageTitle   :: Text
     , rssImageLink    :: URLText -- URL that the image resource should be an href to.
     , rssImageWidth   :: Maybe Integer
     , rssImageHeight  :: Maybe Integer
     , rssImageDesc    :: Maybe Text
     , rssImageOther   :: [XML.Element]
     }
     deriving (Show)

data RSSCloud
 = RSSCloud
     { rssCloudDomain   :: Maybe Text
     , rssCloudPort     :: Maybe Text -- on purpose (i.e., not an int)
     , rssCloudPath     :: Maybe Text
     , rssCloudRegister :: Maybe Text
     , rssCloudProtocol :: Maybe Text
     , rssCloudAttrs    :: [(XML.Name,Text)]
     }
     deriving (Show)

data RSSTextInput
 = RSSTextInput
     { rssTextInputTitle :: Text
     , rssTextInputDesc  :: Text
     , rssTextInputName  :: Text
     , rssTextInputLink  :: URLText
     , rssTextInputAttrs :: [(XML.Name,Text)]
     , rssTextInputOther :: [XML.Element]
     }
     deriving (Show)

-- * Default Constructors:

nullRSS :: Text -- ^channel title
        -> URLText -- ^channel link
        -> RSS
nullRSS title link = 
  RSS 
    { rssVersion = "2.0"
    , rssAttrs   = []
    , rssChannel = nullChannel title link
    , rssOther   = []
    }

nullChannel :: Text -- ^rssTitle
            -> URLText -- ^rssLink
            -> RSSChannel
nullChannel title link = 
  RSSChannel
     { rssTitle        = title
     , rssLink         = link
     , rssDescription  = title
     , rssItems        = []
     , rssLanguage     = Nothing
     , rssCopyright    = Nothing
     , rssEditor       = Nothing
     , rssWebMaster    = Nothing
     , rssPubDate      = Nothing
     , rssLastUpdate   = Nothing
     , rssCategories   = []
     , rssGenerator    = Nothing
     , rssDocs         = Nothing
     , rssCloud        = Nothing
     , rssTTL          = Nothing
     , rssImage        = Nothing
     , rssRating       = Nothing
     , rssTextInput    = Nothing
     , rssSkipHours    = Nothing
     , rssSkipDays     = Nothing
     , rssChannelOther = []
     }

nullItem :: Text -- ^title
         -> RSSItem
nullItem title = 
   RSSItem
     { rssItemTitle        = Just title
     , rssItemLink         = Nothing
     , rssItemDescription  = Nothing
     , rssItemAuthor       = Nothing
     , rssItemCategories   = []
     , rssItemComments     = Nothing
     , rssItemEnclosure    = Nothing
     , rssItemGuid         = Nothing
     , rssItemPubDate      = Nothing
     , rssItemSource       = Nothing
     , rssItemAttrs        = []
     , rssItemOther        = []
     }

nullSource :: URLText -- ^source URL
           -> Text    -- ^title
           -> RSSSource
nullSource url title = 
  RSSSource
     { rssSourceURL    = url
     , rssSourceAttrs  = []
     , rssSourceTitle  = title
     }

nullEnclosure :: URLText -- ^enclosure URL
              -> Integer   -- ^enclosure length
              -> Text    -- ^enclosure type
              -> RSSEnclosure
nullEnclosure url len ty = 
  RSSEnclosure
     { rssEnclosureURL     = url
     , rssEnclosureLength  = len
     , rssEnclosureType    = ty
     , rssEnclosureAttrs   = []
     }

newCategory :: Text  -- ^category Value
            -> RSSCategory
newCategory nm = 
  RSSCategory
     { rssCategoryDomain   = Nothing
     , rssCategoryAttrs    = []
     , rssCategoryValue    = nm
     }

nullGuid :: Text -- ^guid value
         -> RSSGuid
nullGuid v = 
  RSSGuid
     { rssGuidPermanentURL = Nothing
     , rssGuidAttrs        = []
     , rssGuidValue        = v
     }

nullPermaGuid :: Text -- ^guid value
              -> RSSGuid
nullPermaGuid v = (nullGuid v){rssGuidPermanentURL=Just True}

nullImage :: URLText -- ^imageURL
          -> Text    -- ^imageTitle
          -> URLText -- ^imageLink
          -> RSSImage
nullImage url title link = 
  RSSImage
     { rssImageURL     = url
     , rssImageTitle   = title
     , rssImageLink    = link
     , rssImageWidth   = Nothing
     , rssImageHeight  = Nothing
     , rssImageDesc    = Nothing
     , rssImageOther   = []
     }

nullCloud :: RSSCloud
nullCloud = 
  RSSCloud
     { rssCloudDomain   = Nothing
     , rssCloudPort     = Nothing
     , rssCloudPath     = Nothing
     , rssCloudRegister = Nothing
     , rssCloudProtocol = Nothing
     , rssCloudAttrs    = []
     }

nullTextInput :: Text    -- ^inputTitle
              -> Text    -- ^inputName
              -> URLText -- ^inputLink
              -> RSSTextInput
nullTextInput title nm link = 
  RSSTextInput
     { rssTextInputTitle = title
     , rssTextInputDesc  = title
     , rssTextInputName  = nm
     , rssTextInputLink  = link
     , rssTextInputAttrs = []
     , rssTextInputOther = []
     }

