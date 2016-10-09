module MarioLike exposing (..)

import Html exposing (Html, div)
import Html.Attributes as Attr
import Html.App as App
import Task
import AnimationFrame
import Window
import Keyboard.Extra


--

import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)


{-|

This is a copy of the original Mario game example previously found on the examples for elm.

I made some modifications, but it still is essentially the same, except now with my library and other textures.
-}



-- Msg


type Msg
    = ScreenSize Window.Size
    | Tick Float
    | Resources Resources.Msg
    | Keys Keyboard.Extra.Msg



-- MODEL


type alias Model =
    { mario : Mario
    , resources : Resources
    , keys : Keyboard.Extra.Model
    , time : Float
    , screen : ( Int, Int )
    , camera : Camera
    }


type alias Mario =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Direction
    }


type Direction
    = Left
    | Right


mario : Mario
mario =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    }


init : ( Model, Cmd Msg )
init =
    let
        ( keys, cmd ) =
            Keyboard.Extra.init
    in
        { mario = mario
        , resources = Resources.init
        , keys = keys
        , time = 0
        , screen = ( 800, 600 )
        , camera = Camera.fixedWidth 15 ( 0, 0 )
        }
            ! [ getScreenSize
              , Cmd.map Resources (Resources.loadTextures [ "images/guy.png", "images/grass.png", "images/cloud_bg.png" ])
              , Cmd.map Keys cmd
              ]


type alias Input =
    { x : Int, y : Int }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScreenSize { width, height } ->
            { model | screen = ( width, height ) } ! []

        Tick dt ->
            { model
                | mario = tick dt model.keys model.mario
                , time = dt + model.time
                , camera = Camera.moveTo ( model.mario.x, model.mario.y + 0.75 ) model.camera
            }
                ! []

        Resources msg ->
            { model | resources = Resources.update msg model.resources } ! []

        Keys keyMsg ->
            let
                ( keys, cmd ) =
                    Keyboard.Extra.update keyMsg model.keys
            in
                { model | keys = keys } ! [ Cmd.map Keys cmd ]


tick : Float -> Keyboard.Extra.Model -> Mario -> Mario
tick dt keys mario =
    let
        arrows =
            Keyboard.Extra.arrows keys
    in
        mario
            |> gravity dt
            |> jump arrows
            |> walk arrows
            |> physics dt


jump : Input -> Mario -> Mario
jump keys mario =
    if keys.y > 0 && mario.vy == 0 then
        { mario | vy = 4.0 }
    else
        mario


gravity : Float -> Mario -> Mario
gravity dt mario =
    { mario
        | vy =
            if mario.y > 0 then
                mario.vy - 9.81 * dt
            else
                0
    }


physics : Float -> Mario -> Mario
physics dt mario =
    { mario
        | x = mario.x + dt * mario.vx
        , y = max 0 (mario.y + dt * mario.vy)
    }


walk : Input -> Mario -> Mario
walk keys mario =
    { mario
        | vx = toFloat keys.x
        , dir =
            if keys.x < 0 then
                Left
            else if keys.x > 0 then
                Right
            else
                mario.dir
    }



-- VIEW


render : Model -> List Renderable
render { mario, resources, camera } =
    List.concat
        [ renderBackground resources
        , [ Render.spriteWithOptions
                { position = ( -10, -10, 0 )
                , size = ( 20, 10 )
                , texture = Resources.getTexture "images/grass.png" resources
                , rotation = 0
                , pivot = ( 0, 0 )
                , tiling = ( 10, 5 )
                }
          , renderMario resources mario
          ]
        ]


renderBackground : Resources -> List Renderable
renderBackground resources =
    [ Render.parallaxScroll
        { z = -0.99
        , texture = Resources.getTexture "images/cloud_bg.png" resources
        , tileWH = ( 1, 1 )
        , scrollSpeed = ( 0.25, 0.25 )
        }
    , Render.parallaxScroll
        { z = -0.98
        , texture = Resources.getTexture "images/cloud_bg.png" resources
        , tileWH = ( 1.4, 1.4 )
        , scrollSpeed = ( 0.5, 0.5 )
        }
    ]


renderMario : Resources -> Mario -> Renderable
renderMario resources { x, y, dir } =
    let
        d =
            if dir == Left then
                -1
            else
                1
    in
        Render.animatedSpriteWithOptions
            { position = ( x, y, 0 )
            , size = ( d * 0.25, 0.8 )
            , texture = Resources.getTexture "images/guy.png" resources
            , bottomLeft = ( 0, 0 )
            , topRight = ( 1, 1 )
            , duration = 1
            , numberOfFrames = 11
            , rotation = 0
            , pivot = ( 0.5, 0 )
            }


view : Model -> Html msg
view ({ time, screen } as model) =
    div [ Attr.style [ ( "overflow", "hidden" ), ( "width", "100%" ), ( "height", "100%" ) ] ]
        [ Game.render
            { camera = model.camera
            , time = time
            , size = screen
            }
            (render model)
        ]


main : Program Never
main =
    App.program
        { update = update
        , init = init
        , view = view
        , subscriptions = subs
        }


getScreenSize : Cmd Msg
getScreenSize =
    Task.perform (\_ -> Debug.crash "won't happen") ScreenSize (Window.size)


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Window.resizes ScreenSize
        , Sub.map Keys Keyboard.Extra.subscriptions
        , AnimationFrame.diffs ((\dt -> dt / 1000) >> Tick)
        ]
