{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.Atom.Feed.Export
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Description: Convert from Atom to XML
--
--------------------------------------------------------------------


module Text.Atom.Feed.Export where

import Text.XML as XML
import Text.Feed.Util
import Text.Atom.Feed

import Data.Text hiding (map)

atom_prefix :: Maybe Text
atom_prefix = Nothing -- Just "atom"

atom_thr_prefix :: Maybe Text
atom_thr_prefix = Just "thr"

atomNS :: Text
atomNS = "http://www.w3.org/2005/Atom"

atomThreadNS :: Text
atomThreadNS = "http://purl.org/syndication/thread/1.0"

xmlns_atom :: Attr
xmlns_atom = (qn, atomNS)
 where
  qn = case atom_prefix of
         Nothing -> Name { nameLocalName   = "xmlns"
                          , nameNamespace    = Nothing
                          , namePrefix = Nothing
                          }
         Just s  -> Name { nameLocalName   = s
                          , nameNamespace    = Nothing   -- XXX: is this ok?
                          , namePrefix = Just "xmlns"
                          }

xmlns_atom_thread :: Attr
xmlns_atom_thread = (qn,atomThreadNS)
 where
  qn = case atom_thr_prefix of
         Nothing -> Name { nameLocalName   = "xmlns"
                          , nameNamespace    = Nothing
                          , namePrefix = Nothing
                          }
         Just s  -> Name { nameLocalName   = s
                          , nameNamespace    = Nothing   -- XXX: is this ok?
                          , namePrefix = Just "xmlns"
                          }

atomName :: Text -> Name
atomName nc   = Name { nameLocalName   = nc
                      , nameNamespace    = Just atomNS
                      , namePrefix = atom_prefix
                      }

atomAttr :: Text -> Text -> Attr
atomAttr x y  = ((atomName x), y)

atomNode :: Text -> [XML.Node] -> XML.Element
atomNode x xs = def { elementName = atomName x, elementNodes = xs }

atomLeaf :: Text -> Text -> XML.Element
atomLeaf tag txt = def
                     { elementName    = atomName tag
                     , elementNodes = [ NodeContent txt ]
                     }

atomThreadName :: Text -> Name
atomThreadName nc =
  Name { nameLocalName   = nc
        , nameNamespace    = Just atomThreadNS
        , namePrefix = atom_thr_prefix
        }

atomThreadAttr :: Text -> Text -> Attr
atomThreadAttr x y  = ((atomThreadName x), y)

atomThreadNode :: Text -> [XML.Node] -> XML.Element
atomThreadNode x xs =
  def { elementName = atomThreadName x, elementNodes = xs }

atomThreadLeaf :: Text -> Text -> XML.Element
atomThreadLeaf tag txt =
  def { elementName = atomThreadName tag
                , elementNodes = [ NodeContent txt ]
                }

--------------------------------------------------------------------------------

xmlFeed :: Feed -> XML.Element
xmlFeed f = ( atomNode "feed"
          $ map NodeElement
          $ [ xmlTitle (feedTitle f) ]
         ++ [ xmlId (feedId f) ]
         ++ [ xmlUpdated (feedUpdated f) ]
         ++ map xmlLink (feedLinks f)
         ++ map xmlAuthor (feedAuthors f)
         ++ map xmlCategory (feedCategories f)
         ++ map xmlContributor (feedContributors f)
         ++ mb xmlGenerator (feedGenerator f)
         ++ mb xmlIcon (feedIcon f)
         ++ mb xmlLogo (feedLogo f)
         ++ mb xmlRights (feedRights f)
         ++ mb xmlSubtitle (feedSubtitle f)
         ++ map xmlEntry (feedEntries f)
         ++ feedOther f )

            { elementAttributes = [xmlns_atom] }


xmlEntry :: Entry -> XML.Element
xmlEntry e  = ( atomNode "entry"
            $ map NodeElement
            $ [ xmlId (entryId e) ]
           ++ [ xmlTitle (entryTitle e) ]
           ++ [ xmlUpdated (entryUpdated e) ]
           ++ map xmlAuthor (entryAuthors e)
           ++ map xmlCategory (entryCategories e)
           ++ mb xmlContent (entryContent e)
           ++ map xmlContributor (entryContributor e)
           ++ map xmlLink (entryLinks e)
           ++ mb  xmlPublished (entryPublished e)
           ++ mb  xmlRights (entryRights e)
           ++ mb  xmlSource (entrySource e)
           ++ mb  xmlSummary (entrySummary e)
	   ++ mb  xmlInReplyTo (entryInReplyTo e)
	   ++ mb  xmlInReplyTotal (entryInReplyTotal e)
           ++ entryOther e )

              { elementAttributes = entryAttrs e }

xmlContent :: EntryContent -> XML.Element
xmlContent cont = case cont of

  TextContent t -> (atomLeaf "content" t)
                      { elementAttributes = [ atomAttr "type" "text" ] }

  HTMLContent t -> (atomLeaf "content" t)
                      { elementAttributes = [ atomAttr "type" "html" ] }

  XHTMLContent x -> (atomNode "content" [ NodeElement x ])
                      { elementAttributes = [ atomAttr "type" "xhtml" ] }

  MixedContent mbTy cs -> (atomNode "content" cs)
                             { elementAttributes = mb (atomAttr "type") mbTy }

  ExternalContent mbTy src -> (atomNode "content" [])
                                 { elementAttributes = [ atomAttr "src" src ]
                                            ++ mb (atomAttr "type") mbTy }


xmlCategory :: Category -> XML.Element
xmlCategory c = (atomNode "category" (map NodeElement (catOther c)))
                  { elementAttributes = [ atomAttr "term" (catTerm c) ]
                               ++ mb (atomAttr "scheme") (catScheme c)
                               ++ mb (atomAttr "label") (catLabel c)
                  }

xmlLink :: Link -> XML.Element
xmlLink l = (atomNode "link" (map NodeElement (linkOther l)))
              { elementAttributes = [ atomAttr "href" (linkHref l) ]
                        ++ mb (atomAttr "rel" . either id id) (linkRel l)
                        ++ mb (atomAttr "type") (linkType l)
                        ++ mb (atomAttr "hreflang") (linkHrefLang l)
                        ++ mb (atomAttr "title") (linkTitle l)
                        ++ mb (atomAttr "length") (linkLength l)
			++ linkAttrs l
              }

xmlSource :: Source -> Element
xmlSource s = atomNode "source" 
            $ map NodeElement
            $ sourceOther s
           ++ map xmlAuthor (sourceAuthors s)
           ++ map xmlCategory (sourceCategories s)
           ++ mb  xmlGenerator (sourceGenerator s)
           ++ mb  xmlIcon      (sourceIcon s)
           ++ mb  xmlId        (sourceId s)
           ++ map xmlLink      (sourceLinks s)
           ++ mb  xmlLogo      (sourceLogo s)
           ++ mb  xmlRights    (sourceRights s)
           ++ mb  xmlSubtitle  (sourceSubtitle s)
           ++ mb  xmlTitle     (sourceTitle s)
           ++ mb  xmlUpdated   (sourceUpdated s)


xmlGenerator :: Generator -> Element
xmlGenerator g = (atomLeaf "generator" (genText g))
                    { elementAttributes = mb (atomAttr "uri") (genURI g)
                               ++ mb (atomAttr "version") (genVersion g)
                    }


xmlAuthor :: Person -> XML.Element
xmlAuthor p = atomNode "author" (xmlPerson p)

xmlContributor :: Person -> XML.Element
xmlContributor c = atomNode "contributor" (xmlPerson c)

xmlPerson :: Person -> [XML.Node]
xmlPerson p = map NodeElement $
            [ atomLeaf "name" (personName p) ]
           ++ mb (atomLeaf "uri")   (personURI p)
           ++ mb (atomLeaf "email") (personEmail p)
           ++ personOther p

xmlInReplyTo :: InReplyTo -> XML.Element
xmlInReplyTo irt = 
     (atomThreadNode "in-reply-to" (replyToContent irt))
		 { elementAttributes = 
		       mb (atomThreadAttr "ref")  (Just $ replyToRef irt)
		    ++ mb (atomThreadAttr "href") (replyToHRef irt)
		    ++ mb (atomThreadAttr "type") (replyToType irt)
		    ++ mb (atomThreadAttr "source") (replyToSource irt)
		    ++ replyToOther irt
		 }

xmlInReplyTotal :: InReplyTotal -> XML.Element
xmlInReplyTotal irt = 
     (atomThreadLeaf "total" (pack $ show $ replyToTotal irt))
		 { elementAttributes = replyToTotalOther irt }

xmlId :: Text -> XML.Element
xmlId i = atomLeaf "id" i

xmlIcon :: URI -> XML.Element
xmlIcon i = atomLeaf "icon" i

xmlLogo :: URI -> XML.Element
xmlLogo l = atomLeaf "logo" l

xmlUpdated :: Date -> XML.Element
xmlUpdated u = atomLeaf "updated" u

xmlPublished :: Date -> XML.Element
xmlPublished p = atomLeaf "published" p

xmlRights :: TextContent -> XML.Element
xmlRights r = xmlTextContent "rights" r

xmlTitle :: TextContent -> XML.Element
xmlTitle r = xmlTextContent "title" r

xmlSubtitle :: TextContent -> XML.Element
xmlSubtitle s = xmlTextContent "subtitle" s

xmlSummary :: TextContent -> XML.Element
xmlSummary s = xmlTextContent "summary" s

xmlTextContent :: Text -> TextContent -> XML.Element
xmlTextContent tg t =
  case t of
    TextText s  -> (atomLeaf tg s) { elementAttributes = [atomAttr "type" "text"] }
    HTMLText s  -> (atomLeaf tg s) { elementAttributes = [atomAttr "type" "html"] }
    XHTMLText e -> (atomNode tg [NodeElement e])
                          { elementAttributes = [atomAttr "type" "xhtml"] }

--------------------------------------------------------------------------------
mb :: (a -> b) -> Maybe a -> [b]
mb _ Nothing = []
mb f (Just x) = [f x]

