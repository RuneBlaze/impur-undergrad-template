module Site.Tmpl (template, blogTemplate, module Impur.Tmpl, categoryLink) where

import Prelude
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes
import Text.Smolder.Markup
import Site.Conf (author, Category(..))
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Unit (Unit(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Text.Smolder.HTML as H
import Impur.FontAwesome (faCss)
import Impur.Types (PostMeta)
import Data.DateTime (Date, DateTime(..), Time)
import Data.List.Types (List(..), (:))
import Impur.Tmpl (codeblock, math, mathblock, faIcon, linkTo)
import Impur.Classes (class TagLike)
import Impur.Limax (limax)

formatDate :: Date -> String
formatDate date =
    let dt = DateTime date (bottom :: Time) in
    let fmt = DayOfMonth  : Placeholder " " : MonthShort : Placeholder " " : (Cons YearFull Nil) in
    format fmt dt

categoryText :: forall t. (TagLike t) => t -> Maybe Int -> String
categoryText c (Just i) = show c <> " " <> show i <> ""
categoryText c Nothing = show c

categoryLink :: forall a t. (TagLike t) => t -> Maybe Int -> Markup a
categoryLink c mb = a ! className "tag is-light" ! href "/archive" $ text $ categoryText c mb

template :: forall a. Markup a -> Markup a
template partial = html ! lang "en" $ do
    let heading = author
    H.head $ do
        H.title $ text heading
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        link ! rel "stylesheet" ! href "/bulmaswatch.min.css"
        link ! rel "stylesheet" ! href "/highlight.min.css"
        link ! rel "stylesheet" ! href "/katex.min.css"
        link ! rel "stylesheet" ! href "/custom.css"
        H.style $ unsafeRawText $ faCss
    body $ do
        section ! className "hero is-primary" $ do
            H.div ! className "hero-head" $ do
                nav ! className "navbar is-transparent" $ do
                    H.div ! className "container" $ do
                        H.div ! className "navbar-brand" $ do
                            a ! className "navbar-item" ! href "/" $ do
                                h1 ! className "title is-5" $ text author
                        H.div ! className "navbar-end" $ do
                            a ! className "navbar-item" ! href "/archive" $ text "Posts"
                            a ! className "navbar-item" ! href "/cv.pdf" $ text "CV"
            H.div ! className "hero-body" $ do
                H.div ! className "container" $ do
                    h1 ! className "title" $ text author
                    h2 ! className "subtitle" $ text "A Static Site Theme for Undergrad Bloggers and Researchers"
        section ! className "section" $ do
            partial

blogTemplate :: forall a t e. Markup a -> (TagLike t) => {
    title :: String,
    published :: Maybe Date,
    category :: Maybe t | e
} -> Markup a
blogTemplate f meta =
    template $ do
        h1 ! className "title" $ text meta.title
        p $ do
            let d = meta.published
            case d of
                Nothing -> pure unit
                Just date -> do
                    strong $ text $ formatDate date
        hr
        H.div ! className "content" $ do
            f
            case meta.category of
                Nothing -> pure unit
                Just cat -> do
                    categoryLink cat Nothing