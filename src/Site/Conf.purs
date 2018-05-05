module Site.Conf (author, Category(..), categories) where

import Prelude
import Impur.Classes (class TagLike)

author :: String
author = "Under Grad"

data Category = CatExample | CatHaskell

categories :: Array Category
categories = [CatExample, CatHaskell]

instance showCategory :: Show Category where
    show CatExample = "Example"
    show CatHaskell = "Haskell"

derive instance eqCategory :: Eq Category

instance tagLikeCategory :: TagLike Category