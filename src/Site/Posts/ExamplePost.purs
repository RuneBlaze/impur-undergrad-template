module Site.Posts.ExamplePost (post) where

import Prelude (($), discard)
import Impur.Types (PostMeta, PostRaw, PostContents, Post, mkDate)
import Site.Tmpl (blogTemplate, math, mathblock, codeblock)
import Text.Smolder.Markup
import Data.Date
import Data.Tuple.Nested ((/\), type (/\))
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes
import Site.Conf (Category(..))
import Data.Maybe (Maybe(..))
import Impur.Classes (class TagLike)

postMeta :: {
    title :: String,
    published :: Maybe Date,
    category :: Maybe Category
}

postMeta = {title: "Example Post", published: mkDate 2018 4 2, category: Just CatHaskell }

contents :: forall a t e. (TagLike t) => {
    title :: String,
    published :: Maybe Date,
    category :: Maybe t | e
} -> Markup a
contents = blogTemplate $ do
    p $ do
      text "We all know the "
      math "\\epsilon"
      text " definition of continuity, and the immediate consequence of equivalent definitions."
      mathblock "\\forall \\epsilon > 0 \\; \\exists \\delta > 0 \\; d(x, x_0) \\implies d'(f(x),f(x_0)) < \\epsilon"
    p $ do
      text "With this, we write the following Clojure code:"
      codeblock "clojure" "(defn sum [coll] (apply + coll))"

post :: forall t13 e.                      
                   { title :: String           
                   , published :: Maybe Date   
                   , category :: Maybe Category
                   } /\                 
                   ({ title :: String          
                    , published :: Maybe Date  
                    , category :: Maybe Category
                    | t13                      
                    }       
                    -> Markup e
                   )
post = postMeta /\ contents