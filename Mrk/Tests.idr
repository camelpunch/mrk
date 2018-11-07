module Mrk.Tests

import Test.Unit
import Mrk

export
main : IO ()
main = runTests tests where

  fixture : Document Html
  fixture = do
    head [] $ do
      title [] "Best site evah"
      stylesheet "foo.css"

    body [ClassNames ["hi"]] $ do
      div [] $ do
        p [] $ text "some paragraph text"
        img "/someimage.jpg" "another sentence" []

  tests : List (IO Bool)
  tests =
       assertEquals
         (show fixture)
         "<html><head><title>Best site evah</title><link rel=\"stylesheet\" href=\"foo.css\" type=\"text/css\"></head><body class=\"hi\"><div><p>some paragraph text</p><img src=\"/someimage.jpg\" alt=\"another sentence\"></div></body></html>"

    :: neutral

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
