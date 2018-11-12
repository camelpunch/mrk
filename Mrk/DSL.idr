module Mrk.DSL

import public Mrk

%default total
%access public export

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

text : String ->
       Document parent
text =
  tell . Text

-- high-level
stylesheet : (href : URI) ->
             {auto placement : Link `HasParent` parent} ->
             Document parent
stylesheet href = link Stylesheet href [MimeType $ Text CSS]
