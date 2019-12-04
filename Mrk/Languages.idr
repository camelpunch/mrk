module Mrk.Languages

%default total
%access public export

data Language
  = English
%name Language lang

Show Language where
  show x = case x of
                English => "en"

Eq Language where
  (==) x y = show x == show y
