module Mrk.NodeNames

%default total
%access public export

data NodeName : Type where
  Html : NodeName
  Head : NodeName
  Title : NodeName
  Link : NodeName
  H : (n : Nat) ->
      {auto min : n `GTE` 1} ->
      {auto max : 6 `GTE` n} ->
      NodeName
  Img : NodeName
  Body : NodeName
  Div : NodeName
  P : NodeName
  Ul : NodeName
  Li : NodeName
  A : NodeName
%name NodeName nodeName

Show NodeName where
  show Html = "html"
  show Head = "head"
  show Title = "title"
  show Link = "link"
  show (H n) = "h" ++ show n
  show Img = "img"
  show Body = "body"
  show Div = "div"
  show P = "p"
  show Ul = "ul"
  show Li = "li"
  show A = "a"
