module Mrk.Tests
import Test.Unit
import Mrk.DSL

%default total

fixture : Document Html
fixture = do
  head [] $ do
    meta (Charset UTF8) []
    title [] "Best site evah"
    stylesheet "foo.css"

  body [ClassNames ["hi"]] $ do
    ul [] $ do
      li [] $ do
        text "inside"
      li [] $ do
        p [] $ do
          a (Just "/contact") [Rel Nofollow] $ text "Contact"

    div [] $ do
      div [] $ do
        text "Yo"
      h1 [] $ do
        text "Main heading"

      h2 [] $ do
        text "Sub heading"

      h3 [] $ do
        text "Sub heading"

      h4 [] $ do
        text "Sub heading"

      h5 [] $ do
        text "Sub heading"

      h6 [] $ do
        text "Sub heading"

      p [] $ text "some paragraph text"
      img "/someimage.jpg" "another sentence" []

export
main : IO ()
main = runTests tests where
  tests : List (IO Bool)
  tests =
       assertEquals
         (show fixture)
         (
         "<html>" ++
         "<head>" ++
         "<meta charset=\"utf-8\">" ++
         "<title>Best site evah</title>" ++
         "<link rel=\"stylesheet\" href=\"foo.css\" type=\"text/css\">" ++
         "</head>" ++
         "<body class=\"hi\">" ++
         "<ul>" ++
         "<li>inside</li>" ++
         "<li><p><a href=\"/contact\" rel=\"nofollow\">Contact</a></p></li>" ++
         "</ul>" ++
         "<div>" ++
         "<div>Yo</div>" ++
         "<h1>Main heading</h1>" ++
         "<h2>Sub heading</h2>" ++
         "<h3>Sub heading</h3>" ++
         "<h4>Sub heading</h4>" ++
         "<h5>Sub heading</h5>" ++
         "<h6>Sub heading</h6>" ++
         "<p>some paragraph text</p>" ++
         "<img src=\"/someimage.jpg\" alt=\"another sentence\">" ++
         "</div>" ++
         "</body>" ++
         "</html>"
         )

    :: neutral

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
