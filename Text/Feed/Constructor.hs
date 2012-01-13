{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Constructor
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Description: Module for an abstraction layer between different kinds of feeds.
--
--------------------------------------------------------------------

module Text.Feed.Constructor
       ( FeedKind(..)
       , newFeed              -- :: FeedKind  -> Feed
       , feedFromRSS          -- :: RSS       -> Feed
       , feedFromAtom         -- :: Atom.Feed -> Feed
       , feedFromRDF          -- :: RSS1.Feed -> Feed
       , feedFromXML          -- :: Element   -> Feed
       , getFeedKind          -- :: Feed      -> FeedKind

       , FeedSetter           -- type _ a = a -> Feed -> Feed
       , addItem              -- :: FeedSetter Item

       , withFeedTitle        -- :: FeedSetter Text
       , withFeedHome         -- :: FeedSetter URLText
       , withFeedHTML         -- :: FeedSetter URLText
       , withFeedDescription  -- :: FeedSetter Text
       , withFeedPubDate      -- :: FeedSetter DateText
       , withFeedLastUpdate   -- :: FeedSetter DateText
       , withFeedDate         -- :: FeedSetter DateText
       , withFeedLogoLink     -- :: FeedSetter URLText
       , withFeedLanguage     -- :: FeedSetter Text
       , withFeedCategories   -- :: FeedSetter [(Text,Maybe Text)]
       , withFeedGenerator    -- :: FeedSetter Text
       , withFeedItems        -- :: FeedSetter [Item]

       , newItem              -- :: FeedKind   -> Item
       , getItemKind          -- :: Item       -> FeedKind
       , atomEntryToItem      -- :: Atom.Entry -> Item
       , rssItemToItem        -- :: RSS.Item   -> Item
       , rdfItemToItem        -- :: RSS1.Item  -> Item

       , ItemSetter           -- type _ a = a -> Item -> Item
       , withItemTitle        -- :: ItemSetter Text
       , withItemLink         -- :: ItemSetter URLText
       , withItemPubDate      -- :: ItemSetter DateText
       , withItemDate         -- :: ItemSetter DateText
       , withItemAuthor       -- :: ItemSetter Text
       , withItemCommentLink  -- :: ItemSetter Text
       , withItemEnclosure    -- :: Text -> Maybe Text -> ItemSetter Integer
       , withItemFeedLink     -- :: Text -> ItemSetter Text
       , withItemId           -- :: Bool   -> ItemSetter Text
       , withItemCategories   -- :: ItemSetter [(Text, Maybe Text)]
       , withItemDescription  -- :: ItemSetter Text
       , withItemRights       -- :: ItemSetter Text
       ) where

import Text.Feed.Types      as Feed.Types

import Text.Atom.Feed       as Atom
import Text.RSS.Syntax      as RSS
import Text.RSS1.Syntax     as RSS1
import Text.DublinCore.Types
import Text.XML as XML
import Text.Feed.Util hiding (filterChildren)

import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Char  ( toLower )
import Data.Text (Text, pack)

-- ToDo:
--
--  - complete set of constructors over feeds
--  - provide a unified treatment of date string reps.
--    (i.e., I know they differ across formats, but ignorant what
--    the constraints are at the moment.)


-- | Construct an empty feed document, intending to output it in 
-- the 'fk' feed format.
newFeed :: FeedKind -> Feed.Types.Feed
newFeed fk = 
  case fk of
    AtomKind -> AtomFeed (Atom.nullFeed "feed-id-not-filled-in"
                                        (TextText "dummy-title")
                                        "dummy-and-bogus-update-date")
    RSSKind mbV -> 
      let def' = (RSS.nullRSS "dummy-title" "default-channel-url") in
      RSSFeed $ fromMaybe def' $ fmap (\ v -> def'{RSS.rssVersion=v}) mbV
    RDFKind mbV -> 
      let def' = (RSS1.nullFeed "default-channel-url" "dummy-title") in
      RSS1Feed $ fromMaybe def' $ fmap (\ v -> def'{RSS1.feedVersion=v}) mbV
      

feedFromRSS :: RSS.RSS -> Feed.Types.Feed
feedFromRSS r = RSSFeed r

feedFromAtom :: Atom.Feed -> Feed.Types.Feed
feedFromAtom f = AtomFeed f

feedFromRDF :: RSS1.Feed -> Feed.Types.Feed
feedFromRDF f = RSS1Feed f

feedFromXML :: XML.Element -> Feed.Types.Feed
feedFromXML f = XMLFeed f
    
getFeedKind :: Feed.Types.Feed -> FeedKind
getFeedKind f = 
  case f of
    Feed.Types.AtomFeed{} -> AtomKind
    Feed.Types.RSSFeed r  -> RSSKind (case RSS.rssVersion r of { "2.0" -> Nothing; v -> Just v})
    Feed.Types.RSS1Feed r -> RDFKind (case RSS1.feedVersion r of { "1.0" -> Nothing; v -> Just v})
    Feed.Types.XMLFeed{}  -> RSSKind (Just "2.0") -- for now, just a hunch..

addItem :: Feed.Types.Item -> Feed.Types.Feed -> Feed.Types.Feed
addItem it f = 
  case (it,f) of
    (Feed.Types.AtomItem e, Feed.Types.AtomFeed fe) -> 
       Feed.Types.AtomFeed fe{Atom.feedEntries=e:Atom.feedEntries fe}
    (Feed.Types.RSSItem e, Feed.Types.RSSFeed r) -> 
       Feed.Types.RSSFeed r{RSS.rssChannel=(RSS.rssChannel r){RSS.rssItems=e:RSS.rssItems (RSS.rssChannel r)}}
    (Feed.Types.RSS1Item e, Feed.Types.RSS1Feed r) -> 
         -- note: do not update the channel item URIs at this point;
         -- will delay doing so until serialization.
       Feed.Types.RSS1Feed r{RSS1.feedItems=e:RSS1.feedItems r}
    _ -> error "addItem: currently unable to automatically convert items from one feed type to another"

withFeedItems :: FeedSetter [Feed.Types.Item]
withFeedItems is fe = 
 foldr addItem
   (case fe of
      Feed.Types.AtomFeed f -> Feed.Types.AtomFeed 
          f{Atom.feedEntries=[]}
      Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  
          f{rssChannel=(rssChannel f){rssItems=[]}}
      Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed 
          f{feedItems=[]}
      _ -> error "withFeedItems: currently unable to set items in an XMLFeed")
   is

newItem :: FeedKind -> Feed.Types.Item
newItem fk = 
  case fk of
    AtomKind  -> Feed.Types.AtomItem $
      Atom.nullEntry "entry-id-not-filled-in"
                     (TextText "dummy-entry-title")
                     "dummy-and-bogus-entry-update-date"
    RSSKind{} -> Feed.Types.RSSItem $
      RSS.nullItem "dummy-rss-item-title"
    RDFKind{} -> Feed.Types.RSS1Item $
      RSS1.nullItem "dummy-item-uri"
                    "dummy-item-title"
                    "dummy-item-link"

getItemKind :: Feed.Types.Item -> FeedKind
getItemKind f = 
  case f of
    Feed.Types.AtomItem{} -> AtomKind
    Feed.Types.RSSItem{}  -> RSSKind (Just "2.0") -- good guess..
    Feed.Types.RSS1Item{} -> RDFKind (Just "1.0")
    Feed.Types.XMLItem{}  -> RSSKind (Just "2.0")

type FeedSetter a = a -> Feed.Types.Feed -> Feed.Types.Feed

withFeedTitle :: FeedSetter Text
withFeedTitle tit fe = 
  case fe of
   Feed.Types.AtomFeed f -> Feed.Types.AtomFeed f{feedTitle=TextText tit}
   Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  f{rssChannel=(rssChannel f){rssTitle=tit}}
   Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed f{feedChannel=(feedChannel f){channelTitle=tit}}
   Feed.Types.XMLFeed  f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elementName e == unqual "channel")
         then Just (mapMaybeChildren (\ e2 -> 
                        if (elementName e2 == unqual "title")
                         then Just (toElement "title" tit)
                         else Nothing) e)
         else Nothing) f

withFeedHome :: FeedSetter URLText
withFeedHome url fe = 
  case fe of
   Feed.Types.AtomFeed f -> Feed.Types.AtomFeed f{feedLinks=newSelf:Atom.feedLinks f}
      -- ToDo: fix, the <link> element is for the HTML home of the channel, not the
      -- location of the feed itself. Struggling to find if there is a common way
      -- to represent this outside of RSS 2.0 standard elements..
   Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  f{rssChannel=(rssChannel f){rssLink=url}}
   Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed f{feedChannel=(feedChannel f){channelURI=url}}
   Feed.Types.XMLFeed  f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elementName e == unqual "channel")
         then Just (mapMaybeChildren (\ e2 -> 
                        if (elementName e2 == unqual "link")
                         then Just (toElement "link" url)
                         else Nothing) e)
         else Nothing) f
 where
  newSelf = (nullLink url){ linkRel=Just (Left "self")
                          , linkType=Just "application/atom+xml" 
                          }

-- | 'withFeedHTML' sets the URL where an HTML version of the
-- feed is published.
withFeedHTML :: FeedSetter URLText
withFeedHTML url fe = 
  case fe of
   Feed.Types.AtomFeed f -> Feed.Types.AtomFeed f{feedLinks=newAlt:Atom.feedLinks f}
   Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  f{rssChannel=(rssChannel f){rssLink=url}}
   Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed f{feedChannel=(feedChannel f){channelLink=url}}
   Feed.Types.XMLFeed  f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elementName e == unqual "channel")
         then Just (mapMaybeChildren (\ e2 -> 
                        if (elementName e2 == unqual "link")
                         then Just (toElement "link" url)
                         else Nothing) e)
         else Nothing) f
 where
  newAlt = (nullLink url){ linkRel=Just (Left "alternate")
                          , linkType=Just "text/html" 
                          }

-- | 'withFeedHTML' sets the URL where an HTML version of the
-- feed is published.
withFeedDescription :: FeedSetter Text
withFeedDescription desc fe = 
  case fe of
   Feed.Types.AtomFeed f -> Feed.Types.AtomFeed 
      f{feedSubtitle=Just (TextText desc)}
   Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  
      f{rssChannel=(rssChannel f){rssDescription=desc}}
   Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed 
      f{feedChannel=(feedChannel f){channelDesc=desc}}
   Feed.Types.XMLFeed  f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elementName e == unqual "channel")
         then Just (mapMaybeChildren (\ e2 -> 
                        if (elementName e2 == unqual "description")
                         then Just (toElement "description" desc)
                         else Nothing) e)
         else Nothing) f

withFeedPubDate :: FeedSetter Text
withFeedPubDate dateStr fe = 
  case fe of
   Feed.Types.AtomFeed f -> Feed.Types.AtomFeed 
      f{feedUpdated=dateStr}
   Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  
      f{rssChannel=(rssChannel f){rssPubDate=Just dateStr}}
   Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed $
      case break isDate $ RSS1.channelDC (RSS1.feedChannel f) of
       (as,(dci:bs)) -> 
         f{RSS1.feedChannel=
           (RSS1.feedChannel f)
             {RSS1.channelDC=as++dci{dcText=dateStr}:bs}}
       (_,[]) -> 
         f{RSS1.feedChannel=
           (RSS1.feedChannel f)
             {RSS1.channelDC=
                DCItem{dcElt=DC_Date,dcText=dateStr}:
                  RSS1.channelDC (RSS1.feedChannel f)}}
   Feed.Types.XMLFeed  f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elementName e == unqual "channel")
         then Just (mapMaybeChildren (\ e2 -> 
                        if (elementName e2 == unqual "pubDate")
                         then Just (toElement "pubDate" dateStr)
                         else Nothing) e)
         else Nothing) f
 where
  isDate dc  = dcElt dc == DC_Date

withFeedLastUpdate :: FeedSetter DateText
withFeedLastUpdate dateStr fe = 
  case fe of
   Feed.Types.AtomFeed f -> Feed.Types.AtomFeed 
      f{feedUpdated=dateStr}
   Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  
      f{rssChannel=(rssChannel f){rssLastUpdate=Just dateStr}}
   Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed $
      case break isDate $ RSS1.channelDC (RSS1.feedChannel f) of
       (as,(dci:bs)) -> 
         f{RSS1.feedChannel=
           (RSS1.feedChannel f)
             {RSS1.channelDC=as++dci{dcText=dateStr}:bs}}
       (_,[]) -> 
         f{RSS1.feedChannel=
           (RSS1.feedChannel f)
             {RSS1.channelDC=
                DCItem{dcElt=DC_Date,dcText=dateStr}:
                  RSS1.channelDC (RSS1.feedChannel f)}}
   Feed.Types.XMLFeed  f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elementName e == unqual "channel")
         then Just (mapMaybeChildren (\ e2 -> 
                        if (elementName e2 == unqual "lastUpdate")
                         then Just (toElement "lastUpdate" dateStr)
                         else Nothing) e)
         else Nothing) f
 where
  isDate dc  = dcElt dc == DC_Date


-- | 'withFeedDate dt' is the composition of 'withFeedPubDate' 
-- and 'withFeedLastUpdate', setting both publication date and
-- last update date to 'dt'. Notice that RSS2.0 is the only format
-- supporting both pub and last-update.
withFeedDate :: FeedSetter DateText
withFeedDate dt f = withFeedPubDate dt(withFeedLastUpdate dt f)


withFeedLogoLink :: URLText -> FeedSetter URLText
withFeedLogoLink imgURL lnk fe = 
  case fe of
   Feed.Types.AtomFeed f -> Feed.Types.AtomFeed 
      f{ feedLogo  = Just imgURL
       , feedLinks = newSelf:Atom.feedLinks f
       }
   Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  
      f{ rssChannel=(rssChannel f)
         {rssImage=Just $ 
            RSS.nullImage imgURL (rssTitle (rssChannel f)) lnk}}
   Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed $
      f{ feedImage   = Just $
            RSS1.nullImage imgURL (RSS1.channelTitle (RSS1.feedChannel f)) lnk
       , feedChannel =
          (feedChannel f){channelImageURI=Just imgURL}
       }
   Feed.Types.XMLFeed  f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elementName e == unqual "channel")
         then Just (mapMaybeChildren (\ e2 -> 
                        if (elementName e2 == unqual "image")
                         then Just (toElement "image" [ toElement "url" imgURL
                                                  , toElement "title" title
                                                  , toElement "link" lnk
                                                  ])
                         else Nothing) e)
         else Nothing) f
     where
      title = 
       case fmap (findChild (unqual "title"))
                 (findChild (unqual "channel") f) of
         Just (Just e1) -> strContent e1
         _ -> "feed_title" -- shouldn't happen..

 where
  newSelf = (nullLink lnk){ linkRel=Just (Left "self")
                          , linkType=Just "application/atom+xml" 
                          }


withFeedLanguage :: FeedSetter Text
withFeedLanguage lang fe = 
  case fe of
   Feed.Types.AtomFeed f -> Feed.Types.AtomFeed $
        f{Atom.feedAttrs=((unqual "lang"){namePrefix=Just "xml"}, lang):Atom.feedAttrs f}
   Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  
        f{rssChannel=(rssChannel f){rssLanguage=Just lang}}
   Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed $
      case break isLang $ RSS1.channelDC (RSS1.feedChannel f) of
       (as,(dci:bs)) -> 
         f{RSS1.feedChannel=
           (RSS1.feedChannel f)
             {RSS1.channelDC=as++dci{dcText=lang}:bs}}
       (_,[]) -> 
         f{RSS1.feedChannel=
           (RSS1.feedChannel f)
             {RSS1.channelDC=
                DCItem{dcElt=DC_Language,dcText=lang}:
                  RSS1.channelDC (RSS1.feedChannel f)}}
   Feed.Types.XMLFeed  f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elementName e == unqual "channel")
         then Just (mapMaybeChildren (\ e2 -> 
                        if (elementName e2 == unqual "language")
                         then Just (toElement "language" lang)
                         else Nothing) e)
         else Nothing) f
 where
  isLang dc  = dcElt dc == DC_Language

withFeedCategories :: FeedSetter [(Text,Maybe Text)]
withFeedCategories cats fe = 
  case fe of
    Feed.Types.AtomFeed f -> Feed.Types.AtomFeed
        f{ Atom.feedCategories =
                map ( \ (t,mb) -> (Atom.newCategory t){Atom.catScheme=mb})
                    cats ++ feedCategories f}
    Feed.Types.RSSFeed f  -> Feed.Types.RSSFeed
        f{rssChannel=(rssChannel f){
            RSS.rssCategories=
              map (\ (t,mb) -> (RSS.newCategory t){RSS.rssCategoryDomain=mb})
                  cats ++ RSS.rssCategories (rssChannel f)}}
    Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed
        f{feedChannel=(feedChannel f){
            RSS1.channelDC=
                map (\ (t,_) -> DCItem{dcElt=DC_Subject,dcText=t})
                    cats ++ RSS1.channelDC (feedChannel f)}}
    Feed.Types.XMLFeed f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elementName e == unqual "channel")
         then Just (
            foldr 
             (\ (t,mb) acc -> 
                addChild (toElement "category"
                                (fromMaybe (\x -> [x])
                                    (fmap (\v -> (\ x -> [(unqual "domain", v),x])) mb) $
                                    (unqual "term", t))
                                 ) acc)
             e
             cats)
         else Nothing) f


withFeedGenerator :: FeedSetter (Text,Maybe URLText)
withFeedGenerator (gen,mbURI) fe = 
  case fe of
   Feed.Types.AtomFeed f -> Feed.Types.AtomFeed $
        f{Atom.feedGenerator=Just ((Atom.nullGenerator gen){Atom.genURI=mbURI})}
   Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  
        f{rssChannel=(rssChannel f){rssGenerator=Just gen}}
   Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed $
      case break isSource $ RSS1.channelDC (RSS1.feedChannel f) of
       (as,(dci:bs)) -> 
         f{RSS1.feedChannel=
           (RSS1.feedChannel f)
             {RSS1.channelDC=as++dci{dcText=gen}:bs}}
       (_,[]) -> 
         f{RSS1.feedChannel=
           (RSS1.feedChannel f)
             {RSS1.channelDC=
                DCItem{dcElt=DC_Source,dcText=gen}:
                  RSS1.channelDC (RSS1.feedChannel f)}}
   Feed.Types.XMLFeed  f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elementName e == unqual "channel")
         then Just (mapMaybeChildren (\ e2 -> 
                        if (elementName e2 == unqual "generator")
                         then Just (toElement "generator" gen)
                         else Nothing) e)
         else Nothing) f
 where
  isSource dc  = dcElt dc == DC_Source



-- Item constructors (all the way to the end):

atomEntryToItem :: Atom.Entry -> Feed.Types.Item
atomEntryToItem e = Feed.Types.AtomItem e

rssItemToItem :: RSS.RSSItem -> Feed.Types.Item
rssItemToItem i = Feed.Types.RSSItem i

rdfItemToItem :: RSS1.Item -> Feed.Types.Item
rdfItemToItem i = Feed.Types.RSS1Item i

type ItemSetter a = a -> Feed.Types.Item -> Feed.Types.Item

-- | 'withItemPubDate dt' associates the creation\/ publication date 'dt'
-- with a feed item.
withItemPubDate :: ItemSetter DateText
withItemPubDate dt fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryUpdated=dt}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemPubDate=Just dt}
    Feed.Types.RSS1Item i ->
      case break isDate $ RSS1.itemDC i of
       (as,(dci:bs)) -> Feed.Types.RSS1Item i{RSS1.itemDC=as++dci{dcText=dt}:bs}
       (_,[]) -> Feed.Types.RSS1Item i{RSS1.itemDC=DCItem{dcElt=DC_Date,dcText=dt}:RSS1.itemDC i}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (toElement "pubDate" dt) $
            filterChildren (\ e -> elementName e /= unqual "pubDate")
                           i
 where
  isDate dc  = dcElt dc == DC_Date

-- | 'withItemDate' is a synonym for 'withItemPubDate'.
withItemDate :: ItemSetter DateText
withItemDate dt fi = withItemPubDate dt fi

-- | 'withItemTitle myTitle' associates a new title, 'myTitle',
-- with a feed item.
withItemTitle :: ItemSetter Text
withItemTitle tit fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryTitle=TextText tit}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemTitle=Just tit}
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item  i{RSS1.itemTitle=tit}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (toElement "title" tit) $
            filterChildren (\ e -> elementName e /= unqual "title")
                           i

-- | 'withItemAuthor auStr' associates new author info
-- with a feed item.
withItemAuthor :: ItemSetter Text
withItemAuthor au fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryAuthors=[nullPerson{personName=au,personURI=Just au}]}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemAuthor=Just au}
    Feed.Types.RSS1Item i ->
      case break isAuthor $ RSS1.itemDC i of
       (as,(dci:bs)) -> Feed.Types.RSS1Item i{RSS1.itemDC=as++dci{dcText=au}:bs}
       (_,[]) -> Feed.Types.RSS1Item i{RSS1.itemDC=DCItem{dcElt=DC_Creator,dcText=au}:RSS1.itemDC i}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (toElement "author" au) $
            filterChildren (\ e -> elementName e /= unqual "author")
                           i
 where
  isAuthor dc  = dcElt dc == DC_Creator

-- | 'withItemFeedLink name myFeed' associates the parent feed URL 'myFeed'
-- with a feed item. It is labelled as 'name'.
withItemFeedLink :: Text -> ItemSetter Text
withItemFeedLink tit url fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entrySource=Just Atom.nullSource{sourceId=Just url,sourceTitle=Just (TextText tit)}}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemSource=Just (RSS.nullSource url tit)}
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item  i{RSS1.itemTitle=tit}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (toElement "source" (((unqual "url"), url),tit)) $
            filterChildren (\ e -> elementName e /= unqual "source")
                           i



-- | 'withItemCommentLink url' sets the URL reference to the comment page to 'url'.
withItemCommentLink :: ItemSetter Text
withItemCommentLink url fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryLinks=((nullLink url){linkRel=Just (Left "replies")}):Atom.entryLinks e}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemComments=Just url}
    Feed.Types.RSS1Item i ->
      case break isRel $ RSS1.itemDC i of
       (as,(dci:bs)) -> Feed.Types.RSS1Item i{RSS1.itemDC=as++dci{dcText=url}:bs}
       (_,[]) -> Feed.Types.RSS1Item i{RSS1.itemDC=DCItem{dcElt=DC_Relation,dcText=url}:RSS1.itemDC i}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (toElement "comments" url) $
            filterChildren (\ e -> elementName e /= unqual "comments")
                           i
 where
  isRel dc  = dcElt dc == DC_Relation

-- | 'withItemEnclosure url mbTy len' sets the URL reference to the comment page to 'url'.
withItemEnclosure :: Text -> Maybe Text -> ItemSetter Integer
withItemEnclosure url ty len fi = 
  case fi of
    Feed.Types.AtomItem e -> Feed.Types.AtomItem 
       e{Atom.entryLinks=((nullLink url){linkRel=Just (Left "enclosure")
                                        ,linkType=ty
                                        ,linkLength=Just (pack $ show len)
                                        }):Atom.entryLinks e}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemEnclosure=Just (nullEnclosure url len (fromMaybe "text/html" ty))}
    Feed.Types.RSS1Item i -> Feed.Types.RSS1Item 
          i{RSS1.itemContent=nullContentInfo{ contentURI=Just url
                                            , contentFormat=ty
                                            }:RSS1.itemContent i}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild ((toElement "enclosure" url)
          {elementAttributes= [ ((unqual "length"), "0")
                      , ((unqual "type"), (fromMaybe "text/html" ty))
                      ]}) $
            filterChildren (\ e -> elementName e /= unqual "enclosure")
                           i


-- | 'withItemId isURL id' associates new unique identifier with a feed item.
-- If 'isURL' is 'True', then the id is assumed to point to a valid web resource.
withItemId :: Bool -> ItemSetter Text
withItemId isURL idS fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryId=idS}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemGuid=Just (nullGuid idS){rssGuidPermanentURL=Just isURL}}
    Feed.Types.RSS1Item i ->
      case break isId $ RSS1.itemDC i of
       (as,(dci:bs)) -> Feed.Types.RSS1Item i{RSS1.itemDC=as++dci{dcText=idS}:bs}
       (_,[]) -> Feed.Types.RSS1Item i{RSS1.itemDC=DCItem{dcElt=DC_Identifier,dcText=idS}:RSS1.itemDC i}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (toElement "guid" (((unqual "isPermaLink"), (pack $ showBool isURL)),idS)) $
            filterChildren (\ e -> elementName e /= unqual "guid")
                           i
 where
  showBool x  = map toLower (show x)
  isId dc     = dcElt dc == DC_Identifier

-- | 'withItemDescription desc' associates a new descriptive string (aka summary)
-- with a feed item.
withItemDescription :: ItemSetter Text
withItemDescription desc fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entrySummary=Just (TextText desc)}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemDescription=Just desc}
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item  i{RSS1.itemDesc=Just desc}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (toElement "description" desc) $
            filterChildren (\ e -> elementName e /= unqual "description")
                           i

-- | 'withItemRights rightStr' associates the rights information 'rightStr'
-- with a feed item.
withItemRights :: ItemSetter Text
withItemRights desc fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryRights=Just (TextText desc)}
     -- Note: per-item copyright information isn't supported by RSS2.0 (and earlier editions),
     -- you can only attach this at the feed/channel level. So, there's not much we can do
     -- except dropping the information on the floor here. (Rolling our own attribute or
     -- extension element is an option, but would prefer if someone else had started that
     -- effort already.
    Feed.Types.RSSItem{}  -> fi
    Feed.Types.RSS1Item i ->
      case break ((==DC_Rights).dcElt) $ RSS1.itemDC i of
       (as,(dci:bs)) -> Feed.Types.RSS1Item i{RSS1.itemDC=as++dci{dcText=desc}:bs}
       (_,[]) -> Feed.Types.RSS1Item i{RSS1.itemDC=DCItem{dcElt=DC_Rights,dcText=desc}:RSS1.itemDC i}
     -- Since we're so far assuming that a shallow XML rep. of an item
     -- is of RSS2.0 ilk, pinning on the rights info is hard (see above.)
    Feed.Types.XMLItem{}  -> fi

-- | 'withItemTitle myLink' associates a new URL, 'myLink',
-- with a feed item.
withItemLink :: ItemSetter URLText
withItemLink url fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryLinks=replaceAlternate url (Atom.entryLinks e)}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemLink=Just url}
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item  i{RSS1.itemLink=url}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (toElement "link" url) $
            filterChildren (\ e -> elementName e /= unqual "link")
                           i
 where
  replaceAlternate _ [] = []
  replaceAlternate x (lr:xs) 
   | toStr (Atom.linkRel lr) == "alternate" = lr{Atom.linkHref=x} : xs
   | otherwise = lr : replaceAlternate x xs

  toStr Nothing = ""
  toStr (Just (Left x)) = x
  toStr (Just (Right x)) = x
    
withItemCategories :: ItemSetter [(Text,Maybe Text)]
withItemCategories cats fi = 
  case fi of
    Feed.Types.AtomItem e -> Feed.Types.AtomItem 
        e{ Atom.entryCategories =
                map ( \ (t,mb) -> (Atom.newCategory t){Atom.catScheme=mb})
                    cats ++ entryCategories e}
    Feed.Types.RSSItem i  -> Feed.Types.RSSItem 
        i{RSS.rssItemCategories=
              map (\ (t,mb) -> (RSS.newCategory t){RSS.rssCategoryDomain=mb})
                  cats ++ rssItemCategories i}
    Feed.Types.RSS1Item i -> Feed.Types.RSS1Item 
         i{RSS1.itemDC=
                map (\ (t,_) -> DCItem{dcElt=DC_Subject,dcText=t})
                    cats ++ RSS1.itemDC i}
    Feed.Types.XMLItem i  -> Feed.Types.XMLItem $
         foldr (\ (t,mb) acc -> 
                  addChild (toElement "category"
                                  (fromMaybe (\x -> [x])
                                             (fmap (\v -> (\ x -> [((unqual "domain"), v),x])) mb) $
                                             ((unqual "term"), t))
                                 ) acc)
               i
               cats

-- helpers..

filterChildren :: (XML.Element -> Bool) -> XML.Element -> XML.Element
filterChildren pre e = 
  case elementNodes e of
    [] -> e
    cs -> e { elementNodes = mapMaybe filterElt cs }
 where
   filterElt xe@(NodeElement el) 
     | pre el    = Just xe
     | otherwise = Nothing
   filterElt xe  = Just xe

addChild :: Element -> Element -> Element
addChild a b = b { elementNodes = NodeElement a : elementNodes b }

mapMaybeChildren :: (Element -> Maybe Element)
                 -> Element
                 -> Element
mapMaybeChildren f e = 
  case elementNodes e of
    [] -> e
    cs -> e { elementNodes = map procElt cs }
 where
   procElt xe@(NodeElement el) =
     case f el of
       Nothing  -> xe
       Just el1 -> NodeElement el1
   procElt xe = xe
