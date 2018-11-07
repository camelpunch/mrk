module Mrk

import public Control.Monad.Writer
import public Control.Monad.Identity
import public Data.List

%default total
%access public export

namespace NodeNames
  data NodeName
    = Html
    | Head
    | Title
    | Link
    | Body
    | Div
    | P
    | Ul
    | Li
  %name NodeName nodeName

  Show NodeName where
    show Html = "html"
    show Head = "head"
    show Title = "title"
    show Link = "link"
    show Body = "body"
    show Div = "div"
    show P = "p"
    show Ul = "ul"
    show Li = "li"

URI : Type
URI = String

data LinkType
  = Alternate
  | Archives
  | Author
  | Bookmark
  | External
  | Feed
  | First
  | Help
  | Icon
  | Index
  | Last
  | License
  | Next
  | Nofollow
  | Noreferrer
  | Pingback
  | Prefetch
  | Prev
  | Search
  | Stylesheet
  | Sidebar
  | Tag
  | Up
%name LinkType rel

Show LinkType where
  show x =
    case x of
         Alternate => "alternate"
         Archives => "archives"
         Author => "author"
         Bookmark => "bookmark"
         External => "external"
         Feed => "feed"
         First => "first"
         Help => "help"
         Icon => "icon"
         Index => "index"
         Last => "last"
         License => "license"
         Next => "next"
         Nofollow => "nofollow"
         Noreferrer => "noreferrer"
         Pingback => "pingback"
         Prefetch => "prefetch"
         Prev => "prev"
         Search => "search"
         Stylesheet => "stylesheet"
         Sidebar => "sidebar"
         Tag => "tag"
         Up => "up"

Eq LinkType where
  (==) x y = show x == show y

namespace MimeType
  data TextSubtype
    = Plain
    | CSS

  Show TextSubtype where
    show Plain = "plain"
    show CSS = "css"

  data MimeType
    = Text TextSubtype

  Show MimeType where
    show (Text subType) = "text/" ++ show subType

data Attribute : Type where
  ClassNames : List String -> Attribute
  Href : URI -> Attribute
  Rel : LinkType -> Attribute
  MimeType : MimeType -> Attribute
%name Attribute attr

showAttr : (name : String) -> (value : String) -> String
showAttr name value = name ++ "=\"" ++ value ++ "\""

Show Attribute where
  show (ClassNames names) = showAttr "class" (unwords names)
  show (Href uri) = showAttr "href" uri
  show (Rel linkType) = showAttr "rel" (show linkType)
  show (MimeType mt) = showAttr "type" (show mt)

Eq Attribute where
  (ClassNames xs) == (ClassNames ys) =
    xs == ys
  (Href x) == (Href y) =
    x == y
  (Rel x) == (Rel y) =
    x == y
  _ == _ =
    False

namespace Elements
  mutual
    data Element : (parent : NodeName) -> Type where
      Generic : (el : NodeName) ->
                (attrs : List Attribute) ->
                (children : Element el) ->
                {auto prf : el `HasParent` parent} ->
                {auto prfAttrsAllowed : disallowedAttrs el attrs = []} ->
                Element parent
      Link : (rel : LinkType) ->
             (href : URI) ->
             (optionalAttrs : List Attribute) ->
             {auto placement : Link `HasParent` parent} ->
             {auto attrsAllowed : disallowedAttrs Link optionalAttrs = []} ->
             Element parent
      Head : List Attribute ->
             (children : Element Head) ->
             {auto oneTitle : numTitles children = 1} ->
             Element Html
      Text : String -> Element parent
      Collection : (existing : Element parent) ->
                   (new : Element parent) ->
                   Element parent

    disallowedAttrs : NodeName -> List Attribute -> List Attribute
    disallowedAttrs nodeName attrs = filter (not . attrPermitted nodeName) attrs where
      attrPermitted : NodeName -> Attribute -> Bool
      attrPermitted Link (Href _) = True
      attrPermitted Link (MimeType _) = True
      attrPermitted _ (ClassNames _) = True
      attrPermitted _ _ = False

    numTitles : Element Head -> Nat
    numTitles (Generic Title xs children) = 1
    numTitles (Collection existing new) = numTitles existing + numTitles new
    numTitles _ = 0

    HasParent : (child : NodeName) -> (parent : NodeName) -> Type
    HasParent child parent = child `Elem` childrenOf parent where
      childrenOf : NodeName -> List NodeName
      childrenOf Html = [Head, Body]
      childrenOf Head = [Link, Title]
      childrenOf Title = []
      childrenOf Link = []
      childrenOf Body = [Div, Ul]
      childrenOf Div = [Ul, Li, Div, P]
      childrenOf P = []
      childrenOf Ul = [Li]
      childrenOf Li = [Div]

  Semigroup (Element parent) where
    (<+>) = Collection

  Monoid (Element parent) where
    neutral = Text ""

  showAttrs : List Attribute -> String
  showAttrs [] = ""
  showAttrs xs = " " ++ unwords (map show xs)

  openTag : NodeName -> List Attribute -> String
  openTag name attrs = "<" ++ show name ++ showAttrs attrs ++ ">"

  selfCloseTag : NodeName -> List Attribute -> String
  selfCloseTag name attrs = "<" ++ show name ++ showAttrs attrs ++ "/>"

  closeTag : NodeName -> String
  closeTag name = "</" ++ show name ++ ">"

  mutual
    openCloseTag : NodeName -> List Attribute -> Element _ -> String
    openCloseTag name attrs children =
      openTag name attrs ++ show children ++ closeTag name

    Show (Element parent) where
      show (Generic el attrs children) =
        openCloseTag el attrs children
      show (Head attrs children) =
        openCloseTag Head attrs children
      show (Link rel href optionalAttrs) =
        selfCloseTag Link (Rel rel :: Href href :: optionalAttrs)
      show (Text x) =
        x
      show (Collection x y) =
        show x ++ show y

Document : (parent : NodeName) -> Type
Document parent = Writer (Element parent) ()

fromDocument : Document parent -> Element parent
fromDocument = snd . runIdentity . runWriterT

Show (Document parent) where
  show children {parent} =
    openCloseTag parent [] (fromDocument children)

head : List Attribute ->
       (children : Document Head) ->
       {auto prf : numTitles (fromDocument children) = 1} ->
       Document Html
head attrs children =
  tell $ Head attrs (fromDocument children)

title : (attrs : List Attribute) ->
        (text : String) ->
        {auto prf : disallowedAttrs Title attrs = []} ->
        Document Head
title attrs text =
  tell $ Generic Title attrs (Text text)

body : (attrs : List Attribute) ->
       (children : Document Body) ->
       {auto prf : disallowedAttrs Body attrs = []} ->
       Document Html
body attrs children =
  tell $ Generic Body attrs (fromDocument children)

div : (attrs : List Attribute) ->
      (children : Document Div) ->
      {auto prf : Div `HasParent` parent} ->
      {auto attrsPrf : disallowedAttrs Div attrs = []} ->
      Document parent
div attrs children =
  tell $ Generic Div attrs (fromDocument children)

p : (attrs : List Attribute) ->
    (children : Document P) ->
    {auto prf : P `HasParent` parent} ->
    {auto attrsPrf : disallowedAttrs P attrs = []} ->
    Document parent
p attrs children =
  tell $ Generic P attrs (fromDocument children)

link : (rel : LinkType) ->
       (href : URI) ->
       (optionalAttrs : List Attribute) ->
       {auto placement : Link `HasParent` parent} ->
       {auto attrsAllowed : disallowedAttrs Link optionalAttrs = []} ->
       Document parent
link rel href optionalAttrs =
  tell $ Link rel href optionalAttrs

stylesheet : (href : URI) ->
             {auto placement : Link `HasParent` parent} ->
             Document parent
stylesheet href = link Stylesheet href [MimeType $ Text CSS]

text : String ->
       Document parent
text =
  tell . Text
