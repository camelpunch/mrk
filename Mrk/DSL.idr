module Mrk.DSL

import public Mrk.Attributes
import public Mrk.Document
import public Mrk.Elements

%default total
%access public export

data ValidAnchor : (href : Maybe URI) -> (attrs : List Attribute) -> Type where
  Hyperlink : IsJust href -> ValidAnchor href attrs
  Placeholder : (IsJust href -> Void) -> (attrs = []) -> ValidAnchor href attrs

namespace Attributeful
  html : (attrs : List Attribute) ->
         (children : Document Html) ->
         {auto attrsPrf : disallowedAttrs Html attrs = []} ->
         Document Root
  html attrs children =
    tell $ Generic Html attrs (fromDocument children)

  head : List Attribute ->
         (children : Document Head) ->
         {auto prf : numTitles (fromDocument children) = 1} ->
         {auto validMeta : ValidMetaPosition (fromDocument children)} ->
         Document Html
  head attrs children =
    tell $ Head attrs (fromDocument children)

  meta : (firstAttr : Attribute) ->
         (attrs : List Attribute) ->
         {auto validAttr : isValidMetaFirstAttr firstAttr = True} ->
         {auto prf : disallowedAttrs Meta attrs = []} ->
         {auto placement : Meta `HasParent` parent} ->
         Document parent
  meta firstAttr attrs =
    tell $ Meta firstAttr attrs

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

  h1 : (attrs : List Attribute) ->
       {auto placement : H 1 `HasParent` parent} ->
       {auto attrsAllowed : disallowedAttrs (H 1) attrs = []} ->
       (children : Document (H 1)) ->
       Document parent
  h1 attrs children =
    tell $ Generic (H 1) attrs (fromDocument children)

  h2 : (attrs : List Attribute) ->
       {auto placement : H 2 `HasParent` parent} ->
       {auto attrsAllowed : disallowedAttrs (H 2) attrs = []} ->
       (children : Document (H 2)) ->
       Document parent
  h2 attrs children =
    tell $ Generic (H 2) attrs (fromDocument children)

  h3 : (attrs : List Attribute) ->
       {auto placement : H 3 `HasParent` parent} ->
       {auto attrsAllowed : disallowedAttrs (H 3) attrs = []} ->
       (children : Document (H 3)) ->
       Document parent
  h3 attrs children =
    tell $ Generic (H 3) attrs (fromDocument children)

  h4 : (attrs : List Attribute) ->
       {auto placement : H 4 `HasParent` parent} ->
       {auto attrsAllowed : disallowedAttrs (H 4) attrs = []} ->
       (children : Document (H 4)) ->
       Document parent
  h4 attrs children =
    tell $ Generic (H 4) attrs (fromDocument children)

  h5 : (attrs : List Attribute) ->
       {auto placement : H 5 `HasParent` parent} ->
       {auto attrsAllowed : disallowedAttrs (H 5) attrs = []} ->
       (children : Document (H 5)) ->
       Document parent
  h5 attrs children =
    tell $ Generic (H 5) attrs (fromDocument children)

  h6 : (attrs : List Attribute) ->
       {auto placement : H 6 `HasParent` parent} ->
       {auto attrsAllowed : disallowedAttrs (H 6) attrs = []} ->
       (children : Document (H 6)) ->
       Document parent
  h6 attrs children =
    tell $ Generic (H 6) attrs (fromDocument children)

  img : (src : URI) ->
        (alt : String) ->
        (optionalAttrs : List Attribute) ->
        {auto placement : Img `HasParent` parent} ->
        {auto attrsAllowed : disallowedAttrs Img optionalAttrs = []} ->
        Document parent
  img src alt optionalAttrs =
    tell $ Img src alt optionalAttrs

  ul : (attrs : List Attribute) ->
       (children : Document Ul) ->
       {auto prf : Ul `HasParent` parent} ->
       {auto attrsPrf : disallowedAttrs Ul attrs = []} ->
       Document parent
  ul attrs children =
    tell $ Generic Ul attrs (fromDocument children)

  li : (attrs : List Attribute) ->
       (children : Document Li) ->
       {auto prf : Li `HasParent` parent} ->
       {auto attrsPrf : disallowedAttrs Li attrs = []} ->
       Document parent
  li attrs children =
    tell $ Generic Li attrs (fromDocument children)

  a : (href : Maybe URI) ->
      (attrs : List Attribute) ->
      (children : Document A) ->
      {auto validAnchor : ValidAnchor href attrs} ->
      {auto placement : A `HasParent` parent} ->
      {auto attrsPrf : disallowedAttrs A attrs = []} ->
      Document parent
  a Nothing _ children = tell $ AnchorPlaceholder (fromDocument children)
  a (Just uri) attrs children = tell $ AnchorHyperlink uri attrs (fromDocument children)

namespace Attributeless

  html : (children : Document Html) ->
         Document Root
  html children = html [] children

  head : (children : Document Head) ->
         {auto prf : numTitles (fromDocument children) = 1} ->
         {auto validMeta : ValidMetaPosition (fromDocument children)} ->
         Document Html
  head children = head [] children

  meta : (firstAttr : Attribute) ->
         {auto validAttr : isValidMetaFirstAttr firstAttr = True} ->
         {auto placement : Meta `HasParent` parent} ->
         Document parent
  meta firstAttr = meta firstAttr []

  title : (text : String) ->
          Document Head
  title text = title [] text

  body : (children : Document Body) ->
         Document Html
  body children = body [] children

  div : (children : Document Div) ->
        {auto prf : Div `HasParent` parent} ->
        Document parent
  div children = div [] children

  p : (children : Document P) ->
      {auto prf : P `HasParent` parent} ->
      Document parent
  p children = p [] children

  link : (rel : LinkType) ->
         (href : URI) ->
         {auto placement : Link `HasParent` parent} ->
         Document parent
  link rel href = link rel href []

  h1 : {auto placement : H 1 `HasParent` parent} ->
       (children : Document (H 1)) ->
       Document parent
  h1 children = h1 [] children

  h2 : {auto placement : H 2 `HasParent` parent} ->
       (children : Document (H 2)) ->
       Document parent
  h2 children = h2 [] children

  h3 : {auto placement : H 3 `HasParent` parent} ->
       (children : Document (H 3)) ->
       Document parent
  h3 children = h3 [] children

  h4 : {auto placement : H 4 `HasParent` parent} ->
       (children : Document (H 4)) ->
       Document parent
  h4 children = h4 [] children

  h5 : {auto placement : H 5 `HasParent` parent} ->
       (children : Document (H 5)) ->
       Document parent
  h5 children = h5 [] children

  h6 : {auto placement : H 6 `HasParent` parent} ->
       (children : Document (H 6)) ->
       Document parent
  h6 children = h6 [] children

  img : (src : URI) ->
        (alt : String) ->
        {auto placement : Img `HasParent` parent} ->
        Document parent
  img src alt = img src alt []

  ul : (children : Document Ul) ->
       {auto prf : Ul `HasParent` parent} ->
       Document parent
  ul children = ul [] children

  li : (children : Document Li) ->
       {auto prf : Li `HasParent` parent} ->
       Document parent
  li children = li [] children

  a : (href : Maybe URI) ->
      (children : Document A) ->
      {auto validAnchor : ValidAnchor href []} ->
      {auto placement : A `HasParent` parent} ->
      Document parent
  a Nothing children = a Nothing [] children
  a (Just uri) children = a (Just uri) [] children

  text : String ->
         Document parent
  text =
    tell . Text

namespace Texty
  li : (s : String) ->
       {auto prf : Li `HasParent` parent} ->
       Document parent
  li s = li [] $ text s

  p : (s : String) ->
      {auto prf : P `HasParent` parent} ->
      Document parent
  p s = p [] $ text s

  h1 : {auto placement : H 1 `HasParent` parent} ->
       (s : String) ->
       Document parent
  h1 s = h1 [] $ text s

  h2 : {auto placement : H 2 `HasParent` parent} ->
       (s : String) ->
       Document parent
  h2 s = h2 [] $ text s

  h3 : {auto placement : H 3 `HasParent` parent} ->
       (s : String) ->
       Document parent
  h3 s = h3 [] $ text s

  h4 : {auto placement : H 4 `HasParent` parent} ->
       (s : String) ->
       Document parent
  h4 s = h4 [] $ text s

  h5 : {auto placement : H 5 `HasParent` parent} ->
       (s : String) ->
       Document parent
  h5 s = h5 [] $ text s

  h6 : {auto placement : H 6 `HasParent` parent} ->
       (s : String) ->
       Document parent
  h6 s = h6 [] $ text s

  a : (href : Maybe URI) ->
      (s : String) ->
      {auto validAnchor : ValidAnchor href []} ->
      {auto placement : A `HasParent` parent} ->
      Document parent
  a Nothing s = a Nothing [] $ text s
  a (Just uri) s = a (Just uri) [] $ text s

namespace HighLevel
  stylesheet : (href : URI) ->
               {auto placement : Link `HasParent` parent} ->
               Document parent
  stylesheet href = link Stylesheet href [MimeType $ Text CSS]
