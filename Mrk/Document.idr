module Mrk.Document

import public Control.Monad.Writer
import public Control.Monad.Identity

import Mrk.Elements

%default total
%access public export

Document : (parent : NodeName) -> Type
Document parent = Writer (Element parent) ()

fromDocument : Document parent -> Element parent
fromDocument = snd . runIdentity . runWriterT

Show (Document parent) where
  show children =
    show (fromDocument children)
