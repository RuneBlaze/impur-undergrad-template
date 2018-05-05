module Site.Pages where

import Prelude
import Site.Posts.ExamplePost as EP
import Data.Tuple.Nested (type (/\), (/\))
import Data.Maybe (Maybe(..), fromMaybe)
import Site.Conf (Category(..), categories)
import Data.Foldable (for_)
import Impur.Classes (class TagLike)
import Impur.Types (PostMeta, categoryCount)
import Data.Date
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes
import Text.Smolder.Markup
import Site.Tmpl (codeblock, linkTo, template, categoryLink, math, mathblock, faIcon)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Data.Array (mapWithIndex)

posts :: Array ({category :: Maybe Category, published :: Maybe Date, title :: String} /\ ({category :: Maybe Category, published :: Maybe Date, title :: String } -> forall e. Markup e))
posts = [EP.post]

categoryPage :: forall t t2 r c. (TagLike t) => (TagLike t2) => t -> Array ({category :: Maybe t2, published :: Maybe Date, title :: String | r} /\ c) -> forall e. Markup e
categoryPage cat psts = template $ do
    h2 $ text $ "Posts with Category: " <> show cat
    ol $ for_ psts \(m /\ _) -> li $
            if (show <$> (m.category :: Maybe t2)) == (show <$> Just cat) then linkTo m else pure unit

postsPage :: forall m a r t t2. (TagLike t) => (TagLike t2) => Array t -> Array ({category :: Maybe t2, published :: Maybe Date, title :: String | r} /\ a) -> Markup m
postsPage cats psts = template $ do
    h1 ! className "title" $ text "Posts Archive"
    hr
    -- very unefficient
    H.div ! className "content" $ do
        for_ cats \cat -> do
            h2 ! className "title" $ text $ show cat
            for_ psts \(m /\ _) -> ol do
                li if (show <$> (m.category :: Maybe t2)) == (show <$> Just cat) then linkTo m else pure unit

index :: forall t r a e. (TagLike t) => Array ({category :: Maybe t | r} /\ a) -> Markup e
index psts = template $ do
    H.div ! className "container" $ do
        H.div ! className "columns" $ do
            H.div ! className "column is-one-quarter" $ do
                figure ! className "image is-square" $ do
                    img ! src "http://via.placeholder.com/256x256"
            H.div ! className "column" $ do
                h1 ! className "title" $ text "Hello, World"
                H.hr
                H.div ! className "content" $ do
                    h1 ! className "title" $ text "Biography"
                    p $ do
                        text "This is an example theme for testing out my custom written "
                        text "static site generator in "
                        a ! href "http://www.purescript.org/" $ text "PureScript"
                        text ", "
                        a ! href "" $ text "https://github.com/RuneBlaze/purescript-impur"
                        text ". "
                        text "This site proudly does not use any JavaScript."
                    p $ do
                        text "This site's css theming is based entirely on "
                        a ! href "https://bulma.io/" $ text "Bulma"
                        text "."
                    h1 ! className "title" $ text "Projects"
                    ul $ do
                        li $ text "Lorem"
                        li $ text "Ipsum"
                    h1 ! className "title" $ text "Posts"
                    ol $ do
                        for_ (mapWithIndex (/\) posts) \(i /\ (m /\ _)) -> li $ linkTo m
                    p $
                        for_ categories \c -> do
                            categoryLink c $ Just (categoryCount c psts)
                            text " "
                    h1 ! className "title" $ text "Contact"
                    p $ do
                        text "You can follow me on "
                        a ! href "" $ do
                            faIcon "twitter"
                            text " twitter"
                        text " and "
                        a ! href "" $ do
                            faIcon "medium"
                            text " medium"
                        text "."
                