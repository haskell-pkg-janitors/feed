{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeSynonymInstances #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Util
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------
module Text.Feed.Util where

import Text.Feed.Types
import System.Time
import System.Locale
import Data.Default
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.XML.Types as XMLTypes
import Text.XML

instance Default Element where def = Element def [] []
instance Default Name where def = Name "" Nothing Nothing

type Attr       = (Name,Text)

-- | 'toFeedDate' translates a calendar time into
-- the format expected by the feed kind.
toFeedDateText :: FeedKind -> ClockTime -> {-Date-}String
toFeedDateText fk ct =
  case fk of
    AtomKind{} -> formatCalendarTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (toUTCTime ct)
    RSSKind{}  -> formatCalendarTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" (toUTCTime ct)
    RDFKind{}  -> formatCalendarTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (toUTCTime ct)

-- xml helpers like those in the old xml and feed packages

-- | Get the text value of an XML element.  This function
-- ignores non-text elements, and concatenates all text elements.
strContent         :: Element -> Text
strContent e        = T.concat $ catMaybes $ map nodeText $ elementNodes e  --concatMap nodeText $ onlyText $ elementNodes e

nodeText :: Node -> Maybe Text
nodeText (NodeContent t) = Just t
nodeText (NodeElement e) = Just $ T.concat $ catMaybes $ map nodeText $ elementNodes e
nodeText _ = Nothing

-- | Select only the elements from a list of XML content.
onlyElems          :: [Node] -> [Element]
onlyElems xs        = [ x | NodeElement x <- xs ]

-- -- | Select only the elements from a parent.
-- elChildren         :: Element -> [Element]
-- elChildren e        = [ x | Elem x <- elContent e ]

-- -- | Select only the text from a list of XML content.
-- onlyText           :: [Node] -> [CData]
-- onlyText xs         = [ x | Text x <- xs ]

-- | Find all immediate children with the given name.
findChildren       :: Name -> Element -> [Element]
findChildren q e    = filterChildren ((q ==) . elementName) e

-- | Filter all immediate children wrt a given predicate.
filterChildren       :: (Element -> Bool) -> Element -> [Element]
filterChildren p e    = filter p (onlyElems (elementNodes e))


-- -- | Filter all immediate children wrt a given predicate over their names.
-- filterChildrenName      :: (Name -> Bool) -> Element -> [Element]
-- filterChildrenName p e   = filter (p.elName) (onlyElems (elContent e))


-- | Find an immediate child with the given name.
findChild          :: Name -> Element -> Maybe Element
findChild q e       = listToMaybe (findChildren q e)

-- -- | Find an immediate child with the given name.
-- filterChild          :: (Element -> Bool) -> Element -> Maybe Element
-- filterChild p e       = listToMaybe (filterChildren p e)

-- -- | Find an immediate child with name matching a predicate.
-- filterChildName      :: (Name -> Bool) -> Element -> Maybe Element
-- filterChildName p e   = listToMaybe (filterChildrenName p e)

-- | Find the left-most occurrence of an element matching given name.
findElement        :: Name -> Element -> Maybe Element
findElement q e     = listToMaybe (findElements q e)

-- -- | Filter the left-most occurrence of an element wrt. given predicate.
-- filterElement        :: (Element -> Bool) -> Element -> Maybe Element
-- filterElement p e     = listToMaybe (filterElements p e)

-- -- | Filter the left-most occurrence of an element wrt. given predicate.
-- filterElementName     :: (Name -> Bool) -> Element -> Maybe Element
-- filterElementName p e  = listToMaybe (filterElementsName p e)

-- | Find all non-nested occurances of an element.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
findElements       :: Name -> Element -> [Element]
findElements qn e = filterElementsName (qn==) e

-- | Find all non-nested occurrences of an element wrt. given predicate.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
filterElements       :: (Element -> Bool) -> Element -> [Element]
filterElements p e
 | p e        = [e]
 | otherwise  = concatMap (filterElements p) $ onlyElems $ elementNodes e

-- | Find all non-nested occurences of an element wrt a predicate over element names.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
filterElementsName       :: (Name -> Bool) -> Element -> [Element]
filterElementsName p e = filterElements (p.elementName) e

-- | Lookup the value of an attribute.
findAttr          :: Name -> Element -> Maybe Text
findAttr x e       = lookupAttr x (elementAttributes e)

-- | Lookup attribute name from list.
lookupAttr        :: Name -> [Attr] -> Maybe Text
lookupAttr x       = lookupAttrBy (x ==)

-- | Lookup the first attribute whose name satisfies the given predicate.
lookupAttrBy       :: (Name -> Bool) -> [Attr] -> Maybe Text
lookupAttrBy p as   = snd `fmap` find (p . fst) as

-- -- | Lookup the value of the first attribute whose name
-- -- satisfies the given predicate.
-- findAttrBy         :: (Name -> Bool) -> Element -> Maybe String
-- findAttrBy p e      = lookupAttrBy p (elAttribs e)

-- Text.XML.Light

-- | Create an unqualified name.
unqual :: Text -> Name
unqual x = Name{nameLocalName=x, nameNamespace=Nothing, namePrefix=Nothing}

mkelement :: Text -> [Attr] -> [Node] -> Element
mkelement name attrs nodes = Element {elementName=unqual name,
                                      elementAttributes=attrs,
                                      elementNodes=nodes}

-- | A smart element constructor which uses the type of its argument
-- to determine what sort of element to make.
class Elementable t where 
  toElement :: Text -> t -> Element

instance Elementable Text where
  toElement n t = mkelement n [] [NodeContent t]

instance Elementable [Element] where
  toElement n es = mkelement n [] $ map NodeElement es

instance Elementable [Attr] where
  toElement n as = mkelement n as []

instance Elementable (Attr,Text) where
  toElement n (a,t) = mkelement n [a] [NodeContent t]

-- Data.XML.Types

elementChildren :: Element -> [Element]
elementChildren e = concatMap ((either (const []) (:[])) . fromXMLElement) $ 
                    XMLTypes.elementChildren $ toXMLElement e
-- ?

showElement :: Element -> Text
showElement = T.concat . XMLTypes.elementText . toXMLElement

