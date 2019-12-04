module Attributes

import public Mrk.Languages
import public Mrk.LinkTypes
import public Mrk.MimeTypes
import public Mrk.Charsets

%default total
%access public export

URI : Type
URI = String

interface AttributeValue a where
  toAttr : a -> String

data Attribute : Type where
  ClassNames : AttributeValue a => List a -> Attribute
  Href : URI -> Attribute
  Lang : Language -> Attribute
  Rel : LinkType -> Attribute
  MimeType : MimeType -> Attribute
  Src : URI -> Attribute
  Alt : String -> Attribute
  Charset : Charset -> Attribute
%name Attribute attr

showAttr : (name : String) -> (value : String) -> String
showAttr name value = name ++ "=\"" ++ value ++ "\""

Show Attribute where
  show (ClassNames names) = showAttr "class" (unwords (map toAttr names))
  show (Href uri) = showAttr "href" uri
  show (Lang lang) = showAttr "lang" (show lang)
  show (Rel linkType) = showAttr "rel" (show linkType)
  show (MimeType mt) = showAttr "type" (show mt)
  show (Src uri) = showAttr "src" uri
  show (Alt alt) = showAttr "alt" alt
  show (Charset UTF8) = showAttr "charset" "utf-8"

Eq Attribute where
  (ClassNames xs) == (ClassNames ys) =
    map toAttr xs == map toAttr ys
  (Href x) == (Href y) =
    x == y
  (Rel x) == (Rel y) =
    x == y
  _ == _ =
    False
