
module App exposing (..)


-- IMPORTS ---------------------------------------------------------------------

import Html exposing ( Html, Attribute, main_, text, div )
import Html.Attributes as Attr exposing ( class )

import Bulma.CDN              as CDN       exposing (..)

-- import Bulma.Helpers          as Helps     exposing (..)

import Bulma.Grid.Columns     as Columns   exposing (..)

import Bulma.Layout.Hero      as Hero      exposing (..)
-- import Bulma.Layout.Footer    as Footer    exposing (..)
import Bulma.Layout.Section   as Section   exposing (..)
import Bulma.Layout.Container as Container exposing (..)

import Bulma.Elements.Image   as Image     exposing (..)
import Bulma.Elements.Title   as Title     exposing (..)

import List exposing ( map, concat )


-- HEPLERS ---------------------------------------------------------------------

ls : a -> List a
ls = flip (::) []


-- ALIASES ---------------------------------------------------------------------

type alias Htmls msg = List (Html msg)

type alias Attrs msg = List (Attribute msg)


-- MODEL -----------------------------------------------------------------------

type alias Model = {}


init : String -> ( Model, Cmd Msg )
init path = {} ! []


-- MSG -------------------------------------------------------------------------

type Msg = NoOp


-- UPDATE ----------------------------------------------------------------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model
  = model ! []


-- VIEW ------------------------------------------------------------------------

-- TODO: should be interactive! to show off our skillsssss

view : Model -> Html Msg
view model
  = main_ []
  <| (::) CDN.stylesheet
  <| concat
    [ hello
    , services
    , team
    , contact
    , foot
    ]

hello : Htmls Msg
hello
  = easyTitle "SOPHWARE"
    |> Title.size1
    |> Title.toHtml
    |> ls
    |> container []
    |> hero []
    |> fullHeight
    |> primary
    |> Hero.toHtml
    |> ls

services : Htmls Msg
services
  = map Section.toHtml
  <| map (section [])
  <| [ frontEnd
    , backEnd
    ]

frontEnd : Container Msg
frontEnd
  = let title_ : Htmls msg
        title_ = Title.toHtmls
                 -- TODO: icon
               <| easyTitleWithSubtitle
                 "Design"
                 "We love clean interfaces."

        beautiful : Html msg
        beautiful = Columns.toHtml
                  <| columns []
                    [ column []
                      [ easyImage [] "http://bulma.io/images/placeholders/256x256.png"
                        |> Image.toHtml
                      ]
                      |> tabletSize4
                      |> desktopSize4
                    , column []
                    <| map Title.toHtml
                    <| map (Title.addClass "has-text-left")
                      [ easyTitle    "Design"                    |> Title.size2
                      , easySubtitle "We love clean interfaces." |> Title.size4
                      ]
                    ]

    in container []
     -- <| (++) title_
     <| [ beautiful
       -- , interactive
       -- , lightweight
       -- , portfolio
       ]

backEnd : Container Msg
backEnd
-- TODO: enterprise/fringe
  = container [] []

team : Htmls Msg
team = []

contact : Htmls Msg
contact = []

foot : Htmls Msg
foot = []


-- MAIN ------------------------------------------------------------------------

main : Program String Model Msg
main
  = Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
