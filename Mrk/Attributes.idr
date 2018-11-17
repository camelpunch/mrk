module Attributes

import public Mrk.LinkTypes
import public Mrk.MimeTypes
import public Mrk.Charsets

%default total
%access public export

URI : Type
URI = String

data Attribute : Type where
  ClassNames : List String -> Attribute
  Href : URI -> Attribute
  Rel : LinkType -> Attribute
  MimeType : MimeType -> Attribute
  Src : URI -> Attribute
  Alt : String -> Attribute
  Charset : Charset -> Attribute
%name Attribute attr

showAttr : (name : String) -> (value : String) -> String
showAttr name value = name ++ "=\"" ++ value ++ "\""

Show Attribute where
  show (ClassNames names) = showAttr "class" (unwords names)
  show (Href uri) = showAttr "href" uri
  show (Rel linkType) = showAttr "rel" (show linkType)
  show (MimeType mt) = showAttr "type" (show mt)
  show (Src uri) = showAttr "src" uri
  show (Alt alt) = showAttr "alt" alt
  show (Charset UTF8) = showAttr "charset" "utf-8"

Eq Attribute where
  (ClassNames xs) == (ClassNames ys) =
    xs == ys
  (Href x) == (Href y) =
    x == y
  (Rel x) == (Rel y) =
    x == y
  _ == _ =
    False
