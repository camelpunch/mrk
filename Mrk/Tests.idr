module Mrk.Tests
import Test.Unit
import Mrk.DSL

fixture : Document Html
fixture = do
  head [] $ do
    title [] "Best site evah"
    stylesheet "foo.css"

  body [ClassNames ["hi"]] $ do
    ul [] $ do
      li [] $ do
        text "inside"

    div [] $ do
      div [] $ do
        text "Yo"
      h 1 [] $ do
        text "Main heading"

      h 2 [] $ do
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
         "<title>Best site evah</title>" ++
         "<link rel=\"stylesheet\" href=\"foo.css\" type=\"text/css\">" ++
         "</head>" ++
         "<body class=\"hi\">" ++
         "<ul><li>inside</li></ul>" ++
         "<div>" ++
         "<div>Yo</div>" ++
         "<h1>Main heading</h1>" ++
         "<h2>Sub heading</h2>" ++
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
