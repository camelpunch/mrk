module Mrk.Elements

import public Data.List

import public Mrk.Attributes
import public Mrk.NodeNames

%default total
%access public export

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
    Img : (src : URI) ->
          (alt : String) ->
          (optionalAttrs : List Attribute) ->
          {auto placement : Img `HasParent` parent} ->
          {auto attrsAllowed : disallowedAttrs Img optionalAttrs = []} ->
          Element parent
    Head : List Attribute ->
           (children : Element Head) ->
           {auto oneTitle : numTitles children = 1} ->
           {auto metaPositionValid : ValidMetaPosition children} ->
           Element Html
    Meta : (firstAttr : Attribute) ->
           (attrs : List Attribute) ->
           {auto prf : isValidMetaFirstAttr firstAttr = True} ->
           {auto attrsAllowed : disallowedAttrs Meta attrs = []} ->
           Element parent
    AnchorHyperlink : (href : URI) ->
                      (attrs : List Attribute) ->
                      (children : Element A) ->
                      {auto placement : A `HasParent` parent} ->
                      {auto attrsAllowed : disallowedAttrs A attrs = []} ->
                      Element parent
    AnchorPlaceholder : (children : Element A) ->
                        {auto placement : A `HasParent` parent} ->
                        Element parent
    Text : String -> Element parent
    Collection : (existing : Element parent) ->
                 (new : Element parent) ->
                 Element parent

  isValidMetaFirstAttr : (attr : Attribute) -> Bool
  isValidMetaFirstAttr (Charset _) = True
  isValidMetaFirstAttr _ = False

  firstChildMetaCharset : Element el -> Bool
  firstChildMetaCharset (Meta (Charset _) _) = True
  firstChildMetaCharset (Collection (Meta (Charset _) _) _) = True
  firstChildMetaCharset _ = False

  metaCharsetCount : (children : Element el) -> Nat
  metaCharsetCount (Meta (Charset _) _) = 1
  metaCharsetCount (Collection (Meta (Charset _) _) rest) = 1 + metaCharsetCount rest
  metaCharsetCount (Collection _ rest) = metaCharsetCount rest
  metaCharsetCount _ = 0

  data ValidMetaPosition : (children : Element el) -> Type where
    FirstChildIsMetaCharset : {auto prf : firstChildMetaCharset children = True} ->
                              ValidMetaPosition children
    NoMetaCharset : {auto prf : metaCharsetCount children = 0} ->
                    ValidMetaPosition children

  disallowedAttrs : NodeName -> List Attribute -> List Attribute
  disallowedAttrs nodeName attrs = filter (not . attrPermitted nodeName) attrs where
    attrPermitted : NodeName -> Attribute -> Bool
    attrPermitted Link (Href _) = True
    attrPermitted Link (MimeType _) = True
    attrPermitted _ (ClassNames _) = True
    attrPermitted A (Rel _) = True
    attrPermitted _ _ = False

  numTitles : Element Head -> Nat
  numTitles (Generic Title xs children) = 1
  numTitles (Collection existing new) = numTitles existing + numTitles new
  numTitles _ = 0

  flowContent : List NodeName
  flowContent =
    [ H 1
    , H 2
    , H 3
    , H 4
    , H 5
    , H 6
    , Div
    , Ul
    , P
    , Img
    ] ++ phrasingContent

  phrasingContent : List NodeName
  phrasingContent =
    interactivePhrasingContent ++ nonInteractivePhrasingContent

  interactivePhrasingContent : List NodeName
  interactivePhrasingContent =
    [ A
    ]

  nonInteractivePhrasingContent : List NodeName
  nonInteractivePhrasingContent =
    [
    ]

  HasParent : (child : NodeName) -> (parent : NodeName) -> Type
  HasParent child parent = child `Elem` childrenOf parent where
    childrenOf : NodeName -> List NodeName
    childrenOf Html = [Head, Body]
    childrenOf Head = [Link, Meta, Title]
    childrenOf Meta = []
    childrenOf Title = []
    childrenOf Link = []
    childrenOf (H n) = phrasingContent
    childrenOf Img = []
    childrenOf Body = flowContent
    childrenOf Div = flowContent
    childrenOf P = phrasingContent
    childrenOf Ul = [Li]
    childrenOf Li = flowContent
    childrenOf A = nonInteractivePhrasingContent

Semigroup (Element parent) where
  (<+>) = Collection

Monoid (Element parent) where
  neutral = Text ""

showAttrs : List Attribute -> String
showAttrs [] = ""
showAttrs xs = " " ++ unwords (map show xs)

openTag : NodeName -> List Attribute -> String
openTag name attrs = "<" ++ show name ++ showAttrs attrs ++ ">"

closeTag : NodeName -> String
closeTag name = "</" ++ show name ++ ">"

mutual
  openCloseTag : NodeName -> List Attribute -> Element _ -> String
  openCloseTag name attrs children =
    openTag name attrs ++ show children ++ closeTag name

  Show (Element parent) where
    show (Generic el attrs (Text "")) =
      openTag el attrs
    show (Generic el attrs children) =
      openCloseTag el attrs children
    show (Head attrs children) =
      openCloseTag Head attrs children
    show (Meta attr attrs) =
      openTag Meta (attr :: attrs)
    show (Link rel href optionalAttrs) =
      openTag Link (Rel rel :: Href href :: optionalAttrs)
    show (Img src alt optionalAttrs) =
      openTag Img (Src src :: Alt alt :: optionalAttrs)
    show (AnchorHyperlink href attrs children) =
      openCloseTag A (Href href :: attrs) children
    show (AnchorPlaceholder children) =
      openCloseTag A [] children
    show (Text x) =
      x
    show (Collection x y) =
       show x ++ show y
