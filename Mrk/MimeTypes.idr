module Mrk.MimeTypes

%default total
%access public export

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
