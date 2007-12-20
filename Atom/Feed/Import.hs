module Atom.Feed.Import where

import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
import Control.Monad (guard)

import Atom.Feed
import Atom.Feed.Export (atomName, atomThreadName)
import Text.XML.Light as XML


pNodes       :: String -> [XML.Element] -> [XML.Element]
pNodes x es   = filter ((atomName x ==) . elName) es

pQNodes      :: QName -> [XML.Element] -> [XML.Element]
pQNodes x es  = filter ((x==) . elName) es

pNode        :: String -> [XML.Element] -> Maybe XML.Element
pNode x es    = listToMaybe (pNodes x es)

pQNode       :: QName -> [XML.Element] -> Maybe XML.Element
pQNode x es   = listToMaybe (pQNodes x es)

pLeaf        :: String -> [XML.Element] -> Maybe String
pLeaf x es    = strContent `fmap` pNode x es

pQLeaf        :: QName -> [XML.Element] -> Maybe String
pQLeaf x es    = strContent `fmap` pQNode x es

pAttr        :: String -> XML.Element -> Maybe String
pAttr x e     = lookup (atomName x) [ (k,v) | Attr k v <- elAttribs e ]

pQAttr       :: QName -> XML.Element -> Maybe String
pQAttr x e    = lookup x [ (k,v) | Attr k v <- elAttribs e ]

pMany        :: String -> (XML.Element -> Maybe a) -> [XML.Element] -> [a]
pMany p f es  = mapMaybe f (pNodes p es)

children     :: XML.Element -> [XML.Element]
children e    = onlyElems (elChildren e)

elementFeed  :: XML.Element -> Maybe Feed
elementFeed e =
  do guard (elName e == atomName "feed")
     let es = children e
     i <- pLeaf "id" es
     t <- pTextContent "title" es
     u <- pLeaf "updated" es
     return Feed
       { feedId           = i
       , feedTitle        = t
       , feedSubtitle     = pTextContent "subtitle" es
       , feedUpdated      = u
       , feedAuthors      = pMany "author" pPerson es
       , feedContributors = pMany "contributor" pPerson es
       , feedCategories   = pMany "category" pCategory es
       , feedGenerator    = pGenerator `fmap` pNode "generator" es
       , feedIcon         = pLeaf "icon" es
       , feedLogo         = pLeaf "logo" es
       , feedRights       = pTextContent "rights" es
       , feedLinks        = pMany "link" pLink es
       , feedEntries      = pMany "entry" pEntry es
       , feedOther        = []  -- XXX: ?
       }

pTextContent :: String -> [XML.Element] -> Maybe TextContent
pTextContent tag es =
  do e <- pNode tag es
     case pAttr "type" e of
       Nothing       -> return (TextString (strContent e))
       Just "text"   -> return (TextString (strContent e))
       Just "html"   -> return (HTMLString (strContent e))
       Just "xhtml"  -> case children e of   -- hmm...
                          [c] -> return (XHTMLString c)
                          _   -> Nothing -- Multiple XHTML children.
       _             -> Nothing          -- Unknown text content type.

pPerson :: XML.Element -> Maybe Person
pPerson e =
  do let es = children e
     name <- pLeaf "name" es   -- or missing "name"
     return Person
       { personName  = name
       , personURI   = pLeaf "uri" es
       , personEmail = pLeaf "email" es
       , personOther = []  -- XXX?
       }

pCategory :: XML.Element -> Maybe Category
pCategory e =
  do term <- pAttr "term" e      -- or missing "term" attribute
     return Category
       { catTerm   = term
       , catScheme = pAttr "scheme" e
       , catLabel  = pAttr "label"  e
       , catOther  = [] -- XXX?
       }

pGenerator :: XML.Element -> Generator
pGenerator e = Generator
   { genURI      = pAttr "href" e
   , genVersion  = pAttr "version" e
   , genText     = strContent e
   }

pSource :: XML.Element -> Source
pSource e =
  let es = children e
  in Source
      { sourceAuthors     = pMany "author" pPerson es
      , sourceCategories  = pMany "category" pCategory es
      , sourceGenerator   = pGenerator `fmap` pNode "generator" es
      , sourceIcon        = pLeaf "icon" es
      , sourceId          = pLeaf "id" es
      , sourceLinks       = pMany "link" pLink es
      , sourceLogo        = pLeaf "logo" es
      , sourceRights      = pTextContent "rights" es
      , sourceSubtitle    = pTextContent "subtitle" es
      , sourceTitle       = pTextContent "title" es
      , sourceUpdated     = pLeaf "updated" es
      , sourceOther       = [] -- XXX ?
      }

pLink :: XML.Element -> Maybe Link
pLink e =
  do uri <- pAttr "href" e
     return Link
       { linkHref     = uri
       , linkRel      = Right `fmap` pAttr "rel" e
       , linkType     = pAttr "type" e
       , linkHrefLang = pAttr "hreflang" e
       , linkTitle    = pAttr "title" e
       , linkLength   = pAttr "length" e
       , linkOther    = []
       }



pEntry :: XML.Element -> Maybe Entry
pEntry e =
  do let es = children e
     i <- pLeaf "id" es
     t <- pTextContent "title" es
     u <- pLeaf "updated" es
     return Entry
       { entryId           = i
       , entryTitle        = t
       , entryUpdated      = u
       , entryAuthors      = pMany "author" pPerson es
       , entryContributor  = pMany "contributor" pPerson es
       , entryCategories   = pMany "category" pCategory es
       , entryContent      = pContent =<< pNode "content" es
       , entryLinks        = pMany "link" pLink es
       , entryPublished    = pLeaf "published" es
       , entryRights       = pTextContent "rights" es
       , entrySource       = pSource `fmap` pNode "source" es
       , entrySummary      = pTextContent "summary" es
       , entryInReplyTo    = pInReplyTo es
       , entryInReplyTotal = pInReplyTotal es
       , entryOther        = [] -- ?
       }

pContent :: XML.Element -> Maybe EntryContent
pContent e =
  case pAttr "type" e of
    Nothing      -> return (TextContent (strContent e))
    Just "text"  -> return (TextContent (strContent e))
    Just "html"  -> return (HTMLContent (strContent e))
    Just "xhtml" ->
      case children e of
        []  -> return (TextContent "")
        [c] -> return (XHTMLContent c)
        _   -> Nothing
    Just ty      ->
      case pAttr "src" e of
        Nothing  -> return (MixedContent (Just ty) (elChildren e))
        Just uri -> return (ExternalContent (Just ty) uri)

pInReplyTotal :: [XML.Element] -> Maybe InReplyTotal
pInReplyTotal es = do
 t <- pQLeaf (atomThreadName "total") es
 case reads t of
   ((x,_):_) -> do
     n <- pQNode (atomThreadName "total") es
     return InReplyTotal
       { replyToTotal      = x
       , replyToTotalOther = elAttribs n
       }
   _ -> fail "no parse"

pInReplyTo :: [XML.Element] -> Maybe InReplyTo
pInReplyTo es = do
 t <- pQNode (atomThreadName "reply-to") es
 case pQAttr (atomThreadName "ref") t of
   Just ref -> 
     return InReplyTo
       { replyToRef     = ref
       , replyToHRef    = pQAttr (atomThreadName "href") t
       , replyToType    = pQAttr (atomThreadName "type") t
       , replyToSource  = pQAttr (atomThreadName "source") t
       , replyToOther   = elAttribs t -- ToDo: snip out matched ones.
       , replyToContent = fromMaybe [] $ elContent t
       }
   _ -> fail "no parse"
