module Mrk.LinkTypes

%default total
%access public export

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
