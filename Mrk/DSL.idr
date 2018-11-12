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

h : (n : Nat) ->
    (attrs : List Attribute) ->
    {auto min : n `GTE` 1} ->
    {auto max : 6 `GTE` n} ->
    {auto placement : H n `HasParent` parent} ->
    {auto attrsAllowed : disallowedAttrs (H n) attrs = []} ->
    (children : Document (H n)) ->
    Document parent
h n attrs children =
  tell $ Generic (H n) attrs (fromDocument children)

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
