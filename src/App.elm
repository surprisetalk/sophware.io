
module App exposing (..)

-- TODO: we should really have, like, a step-by-step project calculator

-- IMPORTS ---------------------------------------------------------------------

import Html exposing ( Html, Attribute, main_, text, div, p, a, wbr, hr, br, ul, blockquote, em, strong, span )
import Html.Attributes as Attr exposing ( id, class, style, href )

import Bulma.CDN as CDN exposing (..)

import Bulma.Components as Components exposing (..)
import Bulma.Elements as Elements exposing (..)
import Bulma.Elements.Icon as Icon exposing ( icon, flask, code, smile_o, comment, star_o, star, heart, cube, cogs, cubes, magic, wrench, users, envelope )
import Bulma.Grid as Grid exposing (..)
import Bulma.Helpers as Helpers exposing (..)
import Bulma.Modifiers as Modifiers exposing (..)
import Bulma.Layout as Layout exposing (..)

-- import Svg exposing ( svg, circle, rect )
-- import Svg.Attributes as SvgAttr exposing ( cx, cy, r, width, height, rx, ry, viewBox, x, y )

import Collage as Collage exposing ( collage, circle, polygon, filled, outlined, defaultLine, move, rotate, ngon, solid )
import Element as Element exposing ( opacity )

import AnimationFrame

import Window

import Mouse exposing ( Position, moves )

import List exposing ( map, map3, concat, intersperse, range )

import Task exposing ( perform )

import Color exposing ( rgba, yellow )

import Time exposing ( Time, inMilliseconds )

import Random exposing ( generate, list, pair, float )


-- HEPLERS ---------------------------------------------------------------------

(=>) = (,)

ls : a -> List a
ls = flip (::) []

sqr : number -> number
sqr x = x * x

-- (%%) : Float -> Int -> Float
-- (%%) n k
--   = 


-- ALIASES ---------------------------------------------------------------------

type alias Htmls msg = List (Html msg)

type alias Attrs msg = List (Attribute msg)


-- MODEL -----------------------------------------------------------------------

type alias Ball = { r : Float, x : Float, y : Float, vx : Float, vy : Float }

type alias Model = { m : Position, ws : Window.Size, balls : List Ball, mesh : List (Float,Float) }


init : String -> ( Model, Cmd Msg )
init path
  = { m = { x = 0, y = 0 }
    , ws = { width  = 1920
           , height = 1080
           }
    , balls = []
    , mesh  = []
    }
    ! [ Window.size |> perform WindowSize
      , generate MeshUpdate <| list 600 <| pair (float -1 1) (float -1 1)
      ]


-- MSG -------------------------------------------------------------------------

type Msg = NoOp
         | MeshUpdate (List (Float,Float))
         | TimeUpdate Time
         | MouseUpdate Position
         | WindowSize Window.Size
         | WindowResize Window.Size


-- UPDATE ----------------------------------------------------------------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({m,ws,balls,mesh} as model)
  = case msg of

      MeshUpdate xs ->

        let distance : (Float,Float) -> (Float,Float) -> Float
            distance (x1,y1) (x2,y2)
              = sqrt <| sqr (x2 - x1) + sqr (y2 - y1)

            sorter_ : (Float,Float) -> List (Float,Float) -> List (Float,Float)
            sorter_ p ps
              = case List.sortBy (distance p) ps of
                  []      -> p :: []
                  q :: qs -> p :: sorter_ q qs

            sorter :  List (Float,Float) -> List (Float,Float)
            sorter ps
              = case ps of
                  []      -> []
                  q :: qs -> sorter_ q qs

        in { model | mesh = xs |> sorter } ! []

      MouseUpdate pos ->

        -- TODO: mouse is pretty intense! let's replace this with scroll

        { model
          | m = pos
          , mesh  = mesh  |> List.indexedMap (\i (x,y) -> ((-1 ^ toFloat i) * sin (toFloat m.x) * 0.001 + x, (-1 ^ toFloat i) * cos (toFloat m.y) * 0.001 + y))
        } ! []

      TimeUpdate time ->

        -- TODO: wiggle the net

        let t : Float
            t = time |> inMilliseconds |> (*) 0.035

            g : Float
            g = 5
            -- g = ( ( toFloat m.y + toFloat ws.height + flor ) / toFloat ws.height ) * 10.0

            fr : Float
            fr = 1

            -- my : Float
            -- my = negate (toFloat m.y - toFloat ws.height - flor)
            --    |> clamp (negate flor) flor

            -- mx : Float
            -- mx = negate wall + toFloat m.x
            --    |> clamp (negate wall) wall

            -- mdx : Float -> Float -> Float
            -- mdx r x = r / (mx - x)

            -- mdy : Float -> Float -> Float
            -- mdy r y = r / (my - y)

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

              -- TODO: x and y should probably be percentages -- it'll make everything so much easier

              = case ( abs y >= flor - r, abs x >= wall - r ) of

                  ( True, _ ) ->

                    {  r = r
                    ,  x = ( vx * t ) + x |> clamp (    r + negate wall) (wall - r    )
                    ,  y = ( vy * t ) + y |> clamp (1 + r + negate flor) (flor - r - 1)
                    , vx = vx - (t * g / r) 
                    , vy = (vy * -1) + fr
                    }

                  ( _, True ) ->

                    {  r = r
                    ,  x = ( vx * t ) + x |> clamp (1 + r + negate wall) (wall - r - 1)
                    ,  y = ( vy * t ) + y |> clamp (    r + negate flor) (flor - r    )
                    , vx = (( vx + fr ) * -1) + (t * g / r)
                    , vy = vy
                    }

                  ( _, _ ) ->

                    {  r = r
                    ,  x = ( vx * t ) + x |> clamp (r + negate wall) (wall - r)
                    ,  y = ( vy * t ) + y |> clamp (r + negate flor) (flor - r)
                    , vx = vx + (t * g / r) 
                    , vy = vy
                    }

        in { model
             | balls = balls |> map baller
             -- , mesh  = mesh  |> map (\(x,y) -> (sin (toFloat m.x) *  0.0001 + x, cos (toFloat m.y) *  0.0001 + y))
             -- , mesh  = mesh  |> map (\(x,y) ->
             --                          case ( 1000 * m.x % 2 == 0, 1000 * m.y % 2 == 0 ) of
             --                            ( True , True  ) -> (sin t *  0.0001 + x, cos t *  0.0001 + y)
             --                            ( True , False ) -> (cos t *  0.0001 + x, sin t *  0.0001 + y)
             --                            ( False, True  ) -> (sin t * -0.0001 + x, cos t * -0.0001 + y)
             --                            ( False, False ) -> (cos t * -0.0001 + x, sin t * -0.0001 + y))
           } ! []
        -- in { model | balls = [ { r = 25, vx = 0, vy = 0, x = mx, y = my } ] } ! []

      WindowResize ws ->

        { model | ws = ws } ! []

      WindowSize ({width,height} as ws) ->

        { model
          | ws = ws
          , balls = range 25 50
                  |> map toFloat
                  |> map3 (\m q n -> {  r = n
                                   ,  x = negate <| toFloat width / 2.0
                                   ,  y = (toFloat height / 25 * toFloat m) - (toFloat height / 2.0)
                                   , vx = 0.0
                                   , vy = q - 5 |> toFloat
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
  -- <| (::) CDN.stylesheet
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
    -- [ textCentered, style [ "min-height" => "90vh" ] ]
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
            [ tab False [] [ a [ href "#design"      ] [ icon Modifiers.Normal [] [ star     ], text "Specialties" ] ]
            , tab False [] [ a [ href "#services"    ] [ icon Modifiers.Normal [] [ wrench   ], text "Services"    ] ]
            , tab False [] [ a [ href "#team"        ] [ icon Modifiers.Normal [] [ users    ], text "Team"        ] ]
            , tab False [] [ a [ href "#contact"     ] [ icon Modifiers.Normal [] [ envelope ], text "Contact"     ] ]
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
    , systemsArchitecture model
    , branding model
    , research model
    , webDevelopment
    ]

interactiveDesign : Model -> Html Msg
interactiveDesign ({ws,balls} as model)
  = hero { heroModifiers | color = Info, size = FullHeight }
    [ id "design" ]
    [ heroBody [ style [ "z-index" => "2" ] ]
      [ container [ style [] ]
        <| easyTitleWithSubtitle False H1
          [ icon Modifiers.Large [] [ magic ], text " UX Design" ]
          [ text "We create experiences." ]
       ++ [ content Modifiers.Medium []
            [ blockquote [ style [ "color" => "#F5F5F5", "background-color" => "rgba(0,0,0,0)" ] ]
              [ text "Any product that needs a manual to work is broken."
              , br [] []
              , br [] []
              , span [ style [ "opacity" => "0.85" ] ] [ text "- Elon Musk" ]
              ]
            ]
          ]
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
  <| map (\{r,x,y} -> circle r |> filled (rgba 255 221 87 ((toFloat width / 2 + x - 0.25) / toFloat width)) |> move (x, y))
  -- <| map (\{r,x,y} -> circle r |> filled (rgba 255 221 87 (abs y / (toFloat height / 2))) |> move (x, y))
  <| balls
    
    
branding : Model -> Html Msg
branding ({ws,balls} as model)
  = hero { heroModifiers | color = Warning, size = FullHeight } []
    [ heroBody [ style [ "z-index" => "2" ] ]
      [ container []
        <| easyTitleWithSubtitle False H1
          [ icon Modifiers.Large [] [ heart ], text " Branding" ]
          [ text "We make companies memorable." ]
       ++ [ content Modifiers.Medium []
            [ -- blockquote [ style [ "border-color" => "rgba(74,74,74,0.6)", "color" => "#4a4a4a", "background-color" => "rgba(0,0,0,0)" ] ]
              -- [ text "Is it really "
              -- , em [] [ text "complex" ]
              -- , text "? Or did we just make it "
              -- , em [] [ text "complicated" ]
              -- , text "?"
              -- , br [] []
              -- , br [] []
              -- , span [ style [ "opacity" => "0.85" ] ] [ text "- Alan Kay" ]
              -- ]
              p [ style [ "max-width" => "500px" ] ]
              [ text "Consequatur nihil aut esse. Libero impedit et autem aut dicta dolore at voluptas. Necessitatibus ducimus autem sapiente amet ad repellat animi."
              ]
            , p [ style [ "max-width" => "500px" ] ]
              [ text "Unde ad ad omnis saepe quas. Magni et aut rem cumque voluptatem architecto quia et. Omnis voluptatem autem nihil rerum. Et dignissimos consectetur dolor consequatur rerum minus sit. Aperiam ut optio praesentium accusantium."
              ]
            ]
          ]
      ]
    -- , div [ style [ "position" => "absolute", "z-index" => "1" ] ]
    --   [ monument ws balls
    --   ] 
    ]

-- monument : Window.Size -> List Ball -> Html Msg
-- monument {width,height} balls
--   = Element.toHtml
--   <| collage width height
--   <| map (\{r,x,y} ->
--           -- ngon (floor r // 5) ((x / toFloat width) * r / 50 * (max (toFloat height) (toFloat width)))
--           ngon 5 (x + (toFloat width / 2))
--           |> outlined { defaultLine | color = rgba 50 50 50 (0.8 - (((toFloat width / 2) + x) / toFloat width)), width = 3 }
--           |> rotate (4 * y / toFloat height)
--           |> move (toFloat width / 2,0))
--   <| List.drop 15
--   -- <| ls
--   -- <| polygon
--   -- <| map (\(x,y) -> (x * 100, y * 100))
--   -- <| map fromPolar
--   -- <| map (\{x,y} -> (x / 100, y / 100))
--   <| balls

systemsArchitecture : Model -> Html Msg
systemsArchitecture ({ws,mesh} as model)
  = hero { heroModifiers | color = Light, size = FullHeight } []
    [ heroBody [ style [ "z-index" => "2" ] ]
      [ container []
        <| easyTitleWithSubtitle False H1
          [ icon Modifiers.Large [] [ cubes ], text " Software Architecture" ]
          [ text "We grow scalable systems." ]
       ++ [ content Modifiers.Medium []
            [ blockquote [ style [ "background-color" => "rgba(0,0,0,0)" ] ]
              [ text "Is it really "
              , em [] [ text "complex" ]
              , text "? Or did we just make it "
              , em [] [ text "complicated" ]
              , text "?"
              , br [] []
              , br [] []
              , span [ style [ "opacity" => "0.85" ] ] [ text "- Alan Kay" ]
              ]
            , p [ style [ "max-width" => "500px" ] ]
              [ text "Consequatur nihil aut esse. Libero impedit et autem aut dicta dolore at voluptas. Necessitatibus ducimus autem sapiente amet ad repellat animi."
              ]
            , p [ style [ "max-width" => "500px" ] ]
              [ text "Unde ad ad omnis saepe quas. Magni et aut rem cumque voluptatem architecto quia et. Omnis voluptatem autem nihil rerum. Et dignissimos consectetur dolor consequatur rerum minus sit. Aperiam ut optio praesentium accusantium."
              ]
            ]
          ]
      ]
    ]

-- randoms : List (Float,Float)
-- randoms
--   = List.map2 (,)
--     (10 ^ 16 * pi |> toString |> String.split "" |> map String.toInt |> map (Result.withDefault 0) |> map toFloat |> map (flip (/) 10.0))
--     (10 ^ 16 * e |> toString |> String.split "" |> map String.toInt |> map (Result.withDefault 0) |> map toFloat |> map (flip (/) 10.0))

web : Window.Size -> List (Float,Float) -> Html Msg
web {width,height} mesh
  = Element.toHtml
  <| collage width height
  <| concat
    -- [ mesh
    --   |> map (\(x,y) -> circle 5 |> filled (rgba 0 209 178 0.8) |> move (toFloat width * x / 2, toFloat height * y / 2))
    [ mesh
      |> map (\(x,y) -> (toFloat width * x / 2, toFloat height * y / 2))
      |> triples
      |> map (polygon >> outlined (solid (rgba 0 209 178 0.8)))
    -- , mesh
    --   |> List.drop 1
    --   |> map (\(x,y) -> (toFloat width * x / 2, toFloat height * y / 2))
    --   |> triples
    --   |> map (polygon >> outlined (solid (yellow)))
    -- , mesh
    --   |> List.drop 2
    --   |> map (\(x,y) -> (toFloat width * x / 2, toFloat height * y / 2))
    --   |> triples
    --   |> map (polygon >> outlined (solid (rgba 255 255 255 0.25)))
    ]

triples : List a -> List (List a)
triples xs
  = case xs of
      x :: y :: z :: xs -> [x,y,z] :: triples xs
      xs                -> ls                 xs

webDevelopment : Html Msg
webDevelopment
  = hero { heroModifiers | color = Light, size = FullHeight } []
    [ heroBody []
      [ container []
        <| easyTitleWithSubtitle False H1
          [ icon Modifiers.Large [] [ code ], text " Software Development" ]
          [ text "We build elegant software." ]
       ++ [ br [] [] ]
       ++ ( ls
          <| columns { columnsModifiers | multiline = False, gapless = True, display = MobileAndBeyond } [ style [ "max-width" => "600px" ] ]
          <| map (easyImage { imageModifiers | size = Just X128 } [ class "gray" ] >> ls >> a [] >> ls >> column { columnModifiers | widths = { mobile = Width4, tablet = Width4, desktop = Width4 } } []) 
            [ "/img/elm"
            , "/img/js.png"
            , "/img/haskell.png"
            -- , "/img/python.png"
            ])
       ++ ( ls
          <| columns { columnsModifiers | multiline = False, gapless = True, display = MobileAndBeyond } [ style [ "max-width" => "600px" ] ]
          <| map (easyImage { imageModifiers | size = Just X128 } [ class "gray" ] >> ls >> a [] >> ls >> column { columnModifiers | widths = { mobile = Width4, tablet = Width4, desktop = Width4 } } []) 
            [ "/img/php.png"
            , "/img/lisp.png"
            , "/img/elixir"
            -- , "/img/python.png"
            ])
        -- TODO: link to github?
      ]
    ]
-- TODO: engineering specialties
-- TODO:   fringe/enterprise + legacy/modern
-- TODO:   languages/frameworks/applications
-- TODO:   favorite tech / proficient in
-- TODO:   architecture/engineering

research : Model -> Html Msg
research ({ws,mesh} as model)
  = hero { heroModifiers | color = Info, size = FullHeight } []
    [ heroBody [ style [ "z-index" => "2" ] ]
      [ container []
        <| easyTitleWithSubtitle False H1
          [ icon Modifiers.Large [] [ flask ], text "Research" ]
          [ text "We solve problems." ]
       ++ [ content Modifiers.Medium [ style [ "color" => "#F5F5F5" ] ]
            [ p [ style [ "max-width" => "500px" ] ]
              [ text "Consequatur nihil aut esse. Libero impedit et autem aut dicta dolore at voluptas. Necessitatibus ducimus autem sapiente amet ad repellat animi."
              ]
            , p [ style [ "max-width" => "500px" ] ]
              [ text "Unde ad ad omnis saepe quas. Magni et aut rem cumque voluptatem architecto quia et. Omnis voluptatem autem nihil rerum. Et dignissimos consectetur dolor consequatur rerum minus sit. Aperiam ut optio praesentium accusantium."
              ]
            ]
          ]
      ]
    , div [ style [ "position" => "absolute", "z-index" => "1" ] ]
      [ web ws mesh
      ] 
    ]
    -- TODO: multiline columns? tiles?
    -- TODO:   mathematics
    -- TODO:   fashion
    -- TODO:   neuroscience
    -- TODO:   finance
    -- TODO:   music

services : Htmls Msg
services
  = [ hero { heroModifiers | color = Dark, size = FullHeight } []
      [ heroBody [ style [ "padding" => "4.5rem 1.5rem" ] ]
        [ container []
          [ title H1 [ textCentered, style [ "margin-left" => "-10px" ] ] [ icon Modifiers.Large [] [ Html.i [ class "fa fa-map-o" ] [] ], text " Services" ]
          , br [] []
          , br [] []
          , columns { columnsModifiers | multiline = True } []
          <| map (column { columnModifiers | widths = { mobile = Width8, tablet = Width6, desktop = Width3 } } []) 
          <| map (\(i,h,t) -> [ title H3 [] [ icon Modifiers.Medium [] [ Html.i [ class <| "fa fa-" ++ i ] [] ], text " ", text h ], t |> map (text >> ls >> p []) |> flip (++) [ br [] [], br [] [] ] |> content Modifiers.Medium [ style [ "color" => "#F5F5F5" ] ] ])
            [ ( "code"
              , "Engineering"
              , [ "Consequatur nihil aut esse. Libero impedit et autem aut dicta dolore at voluptas. Necessitatibus ducimus autem sapiente amet ad repellat animi."
                , "Unde ad ad omnis saepe quas. Magni et aut rem cumque voluptatem architecto quia et. Omnis voluptatem autem nihil rerum. Et dignissimos consectetur dolor consequatur rerum minus sit. Aperiam ut optio praesentium accusantium."
                ]
              )
            , ( "flask"
              , "Studies"
              , [ "Consequatur nihil aut esse. Libero impedit et autem aut dicta dolore at voluptas. Necessitatibus ducimus autem sapiente amet ad repellat animi."
                , "Unde ad ad omnis saepe quas. Magni et aut rem cumque voluptatem architecto quia et. Omnis voluptatem autem nihil rerum. Et dignissimos consectetur dolor consequatur rerum minus sit. Aperiam ut optio praesentium accusantium."
                ]
              )
            , ( "line-chart"
              , "Consulting"
              , [ "Consequatur nihil aut esse. Libero impedit et autem aut dicta dolore at voluptas. Necessitatibus ducimus autem sapiente amet ad repellat animi."
                , "Unde ad ad omnis saepe quas. Magni et aut rem cumque voluptatem architecto quia et. Omnis voluptatem autem nihil rerum. Et dignissimos consectetur dolor consequatur rerum minus sit. Aperiam ut optio praesentium accusantium."
                ]
              )
            , ( "handshake-o"
              , "Partnership"
              , [ "Consequatur nihil aut esse. Libero impedit et autem aut dicta dolore at voluptas. Necessitatibus ducimus autem sapiente amet ad repellat animi."
                , "Unde ad ad omnis saepe quas. Magni et aut rem cumque voluptatem architecto quia et. Omnis voluptatem autem nihil rerum. Et dignissimos consectetur dolor consequatur rerum minus sit. Aperiam ut optio praesentium accusantium."
                ]
              )
            -- , ( "Strategy"
            --   , [ "..." ]
            --   )
            ]
          ]
        ]
      ]
    ]
-- TODO: we need to express that we do partnership, consulting, development, studies, product strategy, etc.

team : Htmls Msg
team
  = [ hero { heroModifiers | color = Info } []
      [ heroBody []
        [ container []
          [ columns { columnsModifiers | multiline = True, display = MobileAndBeyond } []
          <| map (easyImage imageModifiers [ class "gray" ] >> ls >> a [] >> ls >> column { columnModifiers | widths = { mobile = Width4, tablet = Width4, desktop = Width2 } } []) 
            [ "http://bulma.io/images/placeholders/128x128.png"
            , "http://bulma.io/images/placeholders/128x128.png"
            , "http://bulma.io/images/placeholders/128x128.png"
            , "http://bulma.io/images/placeholders/128x128.png"
            , "http://bulma.io/images/placeholders/128x128.png"
            , "http://bulma.io/images/placeholders/128x128.png"
            ]
          ]
        ]
      ]
    ]

contact : Htmls Msg
contact
  = [ hero { heroModifiers | color = Primary } []
      [ heroBody []
        [ container []
          [ columns { columnsModifiers | display = TabletAndBeyond } [ class "is-vcentered" ]
            -- [ column { columnModifiers | offset = Width1, widths = { mobile = Auto, tablet = Width4, desktop = Width4 } } []
              -- [ columns { columnsModifiers | multiline = True, display = MobileAndBeyond } []
              -- <| map (easyImage imageModifiers [ class "gray" ] >> ls >> a [] >> ls >> column { columnModifiers | widths = { mobile = Width4, tablet = Width4, desktop = Width4 } } []) 
              --   [ "http://bulma.io/images/placeholders/128x128.png"
              --   , "http://bulma.io/images/placeholders/128x128.png"
              --   , "http://bulma.io/images/placeholders/128x128.png"
              --   , "http://bulma.io/images/placeholders/128x128.png"
              --   , "http://bulma.io/images/placeholders/128x128.png"
              --   , "http://bulma.io/images/placeholders/128x128.png"
              --   ]
              -- ]
            [ column columnModifiers [ displayByDevice { mobile = InlineFlex, tablet = Block, desktop = Block, widescreen = Block } ]
              [ easyImage imageModifiers [] "http://bulma.io/images/placeholders/1280x960.png"
              ]
            , column columnModifiers []
              <| easyTitleWithSubtitle True H2
                [ icon Modifiers.Large [] [ comment ], text " Contact" ]
                [ text "" ]
           ++ [ field []
                [ controlInput False { controlModifiers | iconLeft = Just ( Modifiers.Small, [], envelope ) } [] [ Attr.placeholder "Email" ] []
                ]
              , field []
                [ controlTextArea False controlModifiers [] [ Attr.placeholder "Say hello! We're friendly." ] []
                ]
              , field []
                [ control controlModifiers []
                  [ button { buttonModifiers | color = Primary, inverted = True, outlined = True } [] [ text "Submit" ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]

foot : Htmls Msg
foot
  = [ footer []
      [ container []
        [ content Modifiers.Medium []
          [ p [] [ text "Consequatur nihil aut esse. Libero impedit et autem aut dicta dolore at voluptas. Necessitatibus ducimus autem sapiente amet ad repellat animi." ]
          ]
        ]
      ]   
    ]


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

