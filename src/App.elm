
module App exposing (..)


-- IMPORTS ---------------------------------------------------------------------

import Html exposing ( Html, Attribute, main_, text, div, p, a, wbr, hr, br, ul )
import Html.Attributes as Attr exposing ( id, class, style, href )

import Bulma.CDN as CDN exposing (..)

import Bulma.Components as Components exposing (..)
import Bulma.Elements as Elements exposing (..)
import Bulma.Elements.Icon as Icon exposing ( icon, flask, code, smile_o, star_o, star, heart, cube, cogs, cubes, magic )
-- import Bulma.Grid as Grid exposing (..)
import Bulma.Helpers as Helpers exposing (..)
import Bulma.Modifiers as Modifiers exposing (..)
import Bulma.Layout as Layout exposing (..)

-- import Svg exposing ( svg, circle, rect )
-- import Svg.Attributes as SvgAttr exposing ( cx, cy, r, width, height, rx, ry, viewBox, x, y )

import Collage as Collage exposing ( collage, circle, filled, move )
import Element as Element

import AnimationFrame

import Window

import Mouse exposing ( Position, moves )

import List exposing ( map, map3, concat, intersperse, range )

import Task exposing ( perform )

import Color exposing ( yellow )

import Time exposing ( Time, inMilliseconds )


-- HEPLERS ---------------------------------------------------------------------

(=>) = (,)

ls : a -> List a
ls = flip (::) []


-- ALIASES ---------------------------------------------------------------------

type alias Htmls msg = List (Html msg)

type alias Attrs msg = List (Attribute msg)


-- MODEL -----------------------------------------------------------------------

type alias Ball = { r : Float, x : Float, y : Float, vx : Float, vy : Float }

type alias Model = { m : Position, ws : Window.Size, balls : List Ball }


init : String -> ( Model, Cmd Msg )
init path
  = { m = { x = 0, y = 0 }
    , ws = { width  = 1920
           , height = 1080
           }
    , balls = []
    }
    ! [ Window.size |> perform WindowSize ]


-- MSG -------------------------------------------------------------------------

type Msg = NoOp
         | TimeUpdate Time
         | MouseUpdate Position
         | WindowSize Window.Size
         | WindowResize Window.Size


-- UPDATE ----------------------------------------------------------------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({m,ws,balls} as model)
  = case msg of

      MouseUpdate pos ->

        { model | m = pos } ! []

      TimeUpdate time ->

        let t : Float
            t = time |> inMilliseconds |> (*) 0.05

            g : Float
            g = 5
            -- g = ( ( toFloat m.y + toFloat ws.height + flor ) / toFloat ws.height ) * 10.0

            fr : Float
            fr = 1

            my : Float
            my = negate (toFloat m.y - toFloat ws.height - flor)
               |> clamp (negate flor) flor

            mx : Float
            mx = negate wall + toFloat m.x
               |> clamp (negate wall) wall

            mdx : Float -> Float -> Float
            mdx r x = r / (mx - x)

            mdy : Float -> Float -> Float
            mdy r y = r / (my - y)

            -- md : Float -> Float -> Float
            -- md x y = sqrt (sqr (x - mx) + sqr (y - my))

            sqr : Float -> Float
            sqr x = x * x

            flor : Float
            flor = toFloat ws.height / 2

            wall : Float
            wall = toFloat ws.width / 2

            baller : Ball -> Ball
            baller {r,x,y,vx,vy}
              -- = case ( r >= md x y, abs y >= flor - r, abs x >= wall - r ) of

              --     ( True, _, _ ) ->

              --       {  r = r
              --       ,  x = ( vx * t ) + x |> clamp (r + negate wall) (wall - r)
              --       ,  y = ( vy * t ) + y |> clamp (r + negate flor) (flor - r)
              --       , vx = vx
              --       , vy = vy - (t * g / r) 
              --       }

              = case ( abs y >= flor - r, abs x >= wall - r ) of

                  ( True, _ ) ->

                    {  r = r
                    ,  x = ( vx * t ) + x |> clamp (    r + negate wall) (wall - r    )
                    ,  y = ( vy * t ) + y |> clamp (1 + r + negate flor) (flor - r - 1)
                    , vx = vx
                    , vy = (vy * -1) - (t * g / r) - fr
                    }

                  ( _, True ) ->

                    {  r = r
                    ,  x = ( vx * t ) + x |> clamp (1 + r + negate wall) (wall - r - 1)
                    ,  y = ( vy * t ) + y |> clamp (    r + negate flor) (flor - r    )
                    , vx = ( vx + fr ) * -1
                    , vy = vy - (t * g / r)
                    }

                  ( _, _ ) ->

                    {  r = r
                    ,  x = ( vx * t ) + x |> clamp (r + negate wall) (wall - r)
                    ,  y = ( vy * t ) + y |> clamp (r + negate flor) (flor - r)
                    , vx = vx
                    , vy = vy - (t * g / r) 
                    }

        in { model | balls = balls |> map baller } ! []
        -- in { model | balls = [ { r = 25, vx = 0, vy = 0, x = mx, y = my } ] } ! []

      WindowResize ws ->

        { model | ws = ws } ! []

      WindowSize ({width,height} as ws) ->

        { model
          | ws = ws
          , balls = range 25 50
                  |> map toFloat
                  |> map3 (\m q n -> {  r = n
                                   ,  x = (toFloat width / 25 * toFloat m) - (toFloat width / 2.0)
                                   ,  y = toFloat height / 2.0
                                   , vx = q - 5 |> toFloat
                                   , vy = 0.0
                                   })
                         (range 1 100)
                         (range 1 100 |> map toFloat |> map ((*) 2.8572385795) |> map floor |> map (flip (%) 20))
        } ! []

      _ ->

        model ! []

-- VIEW ------------------------------------------------------------------------

-- TODO: should be interactive! to show off our skillsssss

-- TODO: what, concretely, can we do for you?

view : Model -> Html Msg
view model
  = main_ [ textLeft ]
  <| (::) CDN.stylesheet
  <| (::) CDN.fontAwesome
  <| (::) hello
  <| concat
    [ specialties model
    , services -- Dark
    , team     -- Primary
    , contact  -- Info
    , foot     -- Dark
    ]

hello : Html Msg
hello
  = hero { heroModifiers | color = Primary, size = Layout.FullHeight, bold = False }
    [ textCentered ]
    [ heroBody []
      [ container []
        [ title H1
          [ style
            [ "font-size" => "6em"
            , "letter-spacing" => "0.35em"
            , "font-weight" => "700"
            ]
          ]
          [ text "SOPH"
          , wbr [] []
          , text "WARE" ]
        ]
      ]
    , heroFoot []
      [ tabs { tabsModifiers | size = Modifiers.Large } [ fullWidth ]
        [ container []
          [ ul []
            [ tab False [] [ a [ href "#specialties" ] [ text "Specialties" ] ]
            , tab False [] [ a [ href "#services"    ] [ text "Services"    ] ]
            , tab False [] [ a [ href "#team"        ] [ text "Team"        ] ]
            , tab False [] [ a [ href "#contact"     ] [ text "Contact"     ] ]
            ]
            -- TODO: icons
            -- TODO: the first three should be rolled-up into a dropdown called "specialties" or something
          ]
        ]
      ]
    ]

specialties : Model -> Htmls Msg
specialties model
  -- = intersperse (hr [] [])
  -- <| map (container [] >> ls >> hero { heroModifiers | color = Warning } [] >> ls >> section NotSpaced [])
  -- = map (\(c,h) -> h |> container [] |> ls |> heroBody [] |> ls |> hero { heroModifiers | color = c, size = FullHeight, bold = False } [])
  -- <| [ ( Info,    interactionDesign )
  --   , ( Warning, branding          )
  --   , ( Dark,    engineering       )
  --   , ( Info,    research          )
  --   ]
  = [ interactiveDesign model
    , systemsArchitecture
    , branding
    , webDevelopment
    , research
    ]

interactiveDesign : Model -> Html Msg
interactiveDesign ({ws,balls} as model)
  = hero { heroModifiers | color = Info, size = FullHeight } []
    [ heroBody [ style [ "z-index" => "2" ] ]
      [ container []
        <| easyTitleWithSubtitle False H1
          [ icon Modifiers.Large [] [ magic ], text " Interactive Design" ]
          [ text "We create engaging experiences." ]
      ]
    , div [ style [ "position" => "absolute", "z-index" => "1" ] ]
      [ ballpit ws balls
      ] 
    ]
  -- TODO: mouse over / tilt red balls

ballpit : Window.Size -> List Ball -> Html Msg
ballpit {width,height} balls
  = Element.toHtml
  <| collage width height
  <| map (\{r,x,y} -> circle r |> filled yellow |> move (x, y))
  <| balls
    
    
branding : Html Msg
branding
  = hero { heroModifiers | color = Warning, size = FullHeight } []
    [ heroBody []
      [ container []
        <| easyTitleWithSubtitle False H1
          [ icon Modifiers.Large [] [ heart ], text " Branding" ]
          [ text "We make companies memorable." ]
      ]
    ]
    

systemsArchitecture : Html Msg
systemsArchitecture
  = hero { heroModifiers | color = Light, size = FullHeight } []
    [ heroBody []
      [ container []
        <| easyTitleWithSubtitle False H1
          [ icon Modifiers.Large [] [ cubes ], text " Systems Architecture" ]
          [ text "We grow scalable systems." ]
      ]
    ]

webDevelopment : Html Msg
webDevelopment
  = hero { heroModifiers | color = Info, size = FullHeight } []
    [ heroBody []
      [ container []
        <| easyTitleWithSubtitle False H1
          [ icon Modifiers.Large [] [ code ], text " Software Development" ]
          [ text "We build elegant software." ]
       -- ++ [ content Modifiers.Medium []
       --      [ p []
       --        [ text "lorem ipsum"
       --          -- TODO: functional
       --          -- TODO: web
       --        ]
       --      ]
       --    ]
      ]
    ]
-- TODO: engineering specialties
-- TODO:   fringe/enterprise + legacy/modern
-- TODO:   languages/frameworks/applications
-- TODO:   favorite tech / proficient in
-- TODO:   architecture/engineering

research : Html Msg
research
  = hero { heroModifiers | color = Light, size = FullHeight } []
    [ heroBody []
      [ container []
        <| easyTitleWithSubtitle False H1
          [ icon Modifiers.Large [] [ flask ], text "Research" ]
          [ text "We solve problems." ]
       ++ [ content Modifiers.Medium []
            [ p []
              [ text "lorem ipsum"
              ]
            ]
          ]
      ]
    ]
    -- TODO: multiline columns? tiles?
    -- TODO:   mathematics
    -- TODO:   fashion
    -- TODO:   neuroscience
    -- TODO:   finance
    -- TODO:   music

services : Htmls Msg
services = []
-- TODO: we need to express that we do partnership, consulting, development, studies, product strategy, etc.

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
        , subscriptions = \model -> Sub.batch
                          [ Window.resizes WindowResize
                          , AnimationFrame.diffs TimeUpdate
                          , Mouse.moves MouseUpdate
                          ]
        }

