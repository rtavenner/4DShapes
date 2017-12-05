import Html exposing (Html, div, text, button, br)
import Html.Attributes exposing (width, height, style, disabled)
import Html.Events exposing (onWithOptions, onInput)
import Html.Lazy
import Keyboard
import Math.Vector4 as Vec4 exposing (vec4, toTuple)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Display4D exposing (..)
import Shapes exposing (..)
import AnimationFrame
import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Time)
import Json.Decode
import Display exposing (display)
import Program

type alias Model = 
    { w : Float
    , rotationMatrix : Mat4
    , object : Tetrahedra
    , shape : Shape
    , color : Color
    , border : Border
    , controls : Controls
    , tutorial : Maybe Tutorial
    }

type Controls = Keys (Set Keyboard.KeyCode) | Animation Animation
type alias Animation = ((Int, Mat4),(List (Int, Mat4)))

type Msg 
    = KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | SetW Float
    | Tick Time
    | Shape Shape
    | Color Color
    | Border Border
    | Noop
    | Tutorial (Maybe Tutorial)
    | Animate (Mat4 -> Animation)

main : Program.Program Model Msg
main = Program.program
    { init = init
    , view = Html.Lazy.lazy2 view
    , update = update
    , subscriptions = subscriptions
    }

init : Model
init =
    { w = 0.001
    , rotationMatrix = mkM4 ((1,0,0,0),(0,0.8,0.6,0),(0,-0.6,0.8,0),(0,0,0,1))
    , object = shape C8 Random True
    , shape = C8
    , color = Random
    , border = True
    , controls = Keys Set.empty
    , tutorial = Just 0
    }

----- VIEW -----

onClick : msg -> Html.Attribute msg
onClick msg =
    onWithOptions "mousedown" 
        { preventDefault = True, stopPropagation = True } 
        (Json.Decode.succeed msg)

view : {width : Int, height : Int} -> Model -> Html Msg
view {width,height} model = 
    div []
        [ case model.tutorial of 
            Nothing -> text ""
            Just t ->
                div [style [("height","100px"), ("background-color","#D8D8D8")]] 
                    [ div 
                        [ style [("float","left"), ("max-width", toString (width-100) ++ "px")]] 
                            (tutorialText t)
                    , div [style [("float","right")]]
                        [ button [onClick (Tutorial (tutorialNext t))] [if t >= 5 then text "Done" else text "Next"]
                        , br [] []
                        , button [onClick (Tutorial (tutorialPrev t))] [text "Prev"]
                        , br [] []
                        , if t >= 5 then text "" else button [onClick (Tutorial Nothing)] [text "Skip"]
                        ]
                    ]
        , Html.Lazy.lazy display 
            ( { width = width - 8
              , height = 
                height - 
                    (case model.tutorial of
                        Just _ -> 218
                        Nothing -> 118)}
            , model.object 
            , model.rotationMatrix 
            , model.w)
        , buttons
            model.tutorial
            model.shape
            model.color
            model.border
        , slider 
            model.w
        ]

buttons : Maybe Tutorial -> Shape -> Color -> Border -> Html Msg
buttons t shape color border =
    let btn msgtype thing txt current =
            button 
                [ onClick (msgtype thing)
                , disabled (current == thing)] 
                [text txt]
        shapeBtn s =
            if enabled (SetShape s) t
            then btn Shape s
            else \_ _ -> text ""
    in div [style [("background-color","#D8D8D8")]]
        [ shapeBtn C5   "simplex"   shape
        , shapeBtn C8   "hypercube" shape
        , shapeBtn C16  "16-cell"   shape
        , shapeBtn C24  "24-cell"   shape
        , shapeBtn C120 "120-cell"  shape
        , shapeBtn C600 "600-cell"  shape
        , if enabled ResetOrientation t
            then button 
                [ onClick <| Animate <| rotateTo Mat4.identity , style [("float","right")] ] 
                [ text "Reset Orientation" ]
            else text ""
        , br [] []
        , btn Color None    "No Color"     color
        , btn Color Random  "Random Color" color
        , btn Color Pattern "Pattern"      color
        , br [] []
        , btn Border False "No Border"     border
        , btn Border True  "Border"        border
        ]

slider : Float -> Html Msg
slider w =
    div [style [("background-color","#D8D8D8")]]
        [ Html.input 
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "-2"
            , Html.Attributes.max "2"
            , Html.Attributes.step "0.01"
            , Html.Attributes.value (toString w)
            , onInput (String.toFloat >> Result.map SetW >> Result.withDefault Noop)
            ] 
            []
        ]

----- UPDATE -----

update : Msg -> Model -> Model
update msg model = 
    case (model.controls, msg) of
        (_, Noop) -> model

        (_, Shape  s) -> {model | shape  = s, object = shape       s     model.color model.border}
        (_, Color  c) -> {model | color  = c, object = shape model.shape       c     model.border}
        (_, Border b) -> {model | border = b, object = shape model.shape model.color       b     }

        (Animation (now, later), Tick _) ->
            if      model.w >  0.02 then {model | w = model.w - 0.02}
            else if model.w < -0.02 then {model | w = model.w + 0.02}
            else if model.w /= 0.001 then {model | w = 0.001}
            else case now of
                (0,_) ->
                    case later of
                        [] ->
                            {model | controls = Keys Set.empty}
                        next :: evenLater ->
                            {model | controls = Animation (next,evenLater)}
                (n, mat) ->
                    rotate mat {model | controls = Animation ((n-1, mat),later)}
        (Animation _, _) -> model

        (Keys keys, KeyDown k) -> {model | controls = Keys (Set.insert k keys)}
        (Keys keys, KeyUp k  ) -> {model | controls = Keys (Set.remove k keys)}
        (Keys keys, Tick _) ->
            Set.foldr 
                (\k m -> 
                    case Dict.get k keyUpdates of
                        Just f -> f m
                        Nothing -> m
                    ) 
                model 
                keys

        (Keys _, SetW w) -> {model | w = w}
        (Keys _, Tutorial Nothing) -> { model | tutorial = Nothing }
        (Keys _, Tutorial (Just t)) -> { model | tutorial = Just t } |> tutorialChanges t
        (Keys _, Animate a) -> { model | controls = Animation <| a model.rotationMatrix }

rotate : Mat4 -> Model -> Model
rotate m model = 
    { model | rotationMatrix = Mat4.mul m model.rotationMatrix } 

rotateEnable : Mat4 -> Model -> Model
rotateEnable m model =
    if enabled Rotate model.tutorial
    then rotate m model
    else model

keyUpdates : Dict Keyboard.KeyCode (Model -> Model)
keyUpdates =
    let c = cos 0.02
        s = sin 0.02
        n = -s
    in Dict.fromList
        [ (37, \model -> { model | w = model.w - 0.02})
        , (39, \model -> { model | w = model.w + 0.02})
        , (87, rotateEnable (mkM4 ((1,0,0,0),(0,c,n,0),(0,s,c,0),(0,0,0,1))))
        , (83, rotateEnable (mkM4 ((1,0,0,0),(0,c,s,0),(0,n,c,0),(0,0,0,1))))
        , (65, rotateEnable (mkM4 ((c,s,0,0),(n,c,0,0),(0,0,1,0),(0,0,0,1))))
        , (68, rotateEnable (mkM4 ((c,n,0,0),(s,c,0,0),(0,0,1,0),(0,0,0,1))))
        , (85, rotateEnable (mkM4 ((1,0,0,0),(0,c,0,n),(0,0,1,0),(0,s,0,c))))
        , (79, rotateEnable (mkM4 ((1,0,0,0),(0,c,0,s),(0,0,1,0),(0,n,0,c))))
        , (73, rotateEnable (mkM4 ((c,0,0,n),(0,1,0,0),(0,0,1,0),(s,0,0,c))))
        , (75, rotateEnable (mkM4 ((c,0,0,s),(0,1,0,0),(0,0,1,0),(n,0,0,c))))
        , (74, rotateEnable (mkM4 ((1,0,0,0),(0,1,0,0),(0,0,c,s),(0,0,n,c))))
        , (76, rotateEnable (mkM4 ((1,0,0,0),(0,1,0,0),(0,0,c,n),(0,0,s,c))))
        , (81, rotateEnable (mkM4 ((c,0,s,0),(0,1,0,0),(n,0,c,0),(0,0,0,1))))
        , (69, rotateEnable (mkM4 ((c,0,n,0),(0,1,0,0),(s,0,c,0),(0,0,0,1))))
        ]

subscriptions : Model -> Sub Msg
subscriptions _ = 
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs Tick
        ]



type alias Tutorial = Int

tutorialNext : Tutorial -> Maybe Tutorial
tutorialNext t =
    if t >= 5
    then Nothing
    else Just (t+1)

tutorialPrev : Tutorial -> Maybe Tutorial
tutorialPrev t =
    if t <= 0
    then Just 0
    else Just (t-1)

tutorialChanges : Tutorial -> (Model -> Model)
tutorialChanges t model =
    { model | controls = 
        case tutorialAnimation t of
            Nothing -> model.controls
            Just a -> Animation <| a model.rotationMatrix
    }
    |> if t < 4 then update (Shape C8) else if t == 4 then update (Shape C5) else identity


tutorialAnimation : Tutorial -> Maybe (Mat4 -> Animation)
tutorialAnimation t =
    case t of
        0 -> 
            Just <| rotateTo (mkM4 ((1,0,0,0),(0,0.8,0.6,0),(0,-0.6,0.8,0),(0,0,0,1)))
        1 -> 
            Just <| \m ->
                append 
                    (m |> rotateTo (mkM4 ((1,0,0,0),(0,0.8,0.6,0),(0,-0.6,0.8,0),(0,0,0,1))))
                    ( ( 45
                      , mkM4 
                        ( (cos (degrees  1), 0, 0, sin (degrees 1))
                        , (              0 , 1, 0,              0 )
                        , (              0 , 0, 1,              0 )
                        , (sin (degrees -1), 0, 0, cos (degrees 1))))
                    , [])
        2 -> 
            Just <| rotateTo (mkM4 ((sqrt(1/2),0,0,sqrt(1/2)),(0,0.8,0.6,0),(0,-0.6,0.8,0),(-(sqrt(1/2)),0,0,sqrt(1/2))))
        _ -> Nothing

tutorialText : Tutorial -> List (Html Msg)
tutorialText t =

    case t of
        0 ->
            [ text "In front of you is a cube."
            , text " This cube is a cross-section of a hypercube (a.k.a. tesseract), in the same way that a square is a cross-section of a cube."
            , br [] []
            , text "You can use the left and right arrow keys, or the slider, to change which slice is being taken."
            , text " If you slide the slider too far, the slice will miss the hypercube, and the screen will be blank."
            , br [] []
            , text "Try it! (Click Next when you are done.)"
            ]
        1 ->
            [ text "But that was a boring cross-section. So I've rotated the hypercube into the fourth dimension."
            , text "Take slices with the slider again. What do you see?"
            , br [] []
            , text "You should see a square prism, that elongates and contracts as you slide the slider. Why?"
            ]
        2 ->
            [ text "Take (or imagine taking) an ordinary cube, and stand it on its edge."
            , text " If you take a horizontal cross-section, you get a rectangle."
            , br [] []
            , text "A slice near the top or bottom of the cube creates a skinny rectangle, but a slice near the middle creates a wide rectangle."
            , br [] []
            , text "A similar thing is happening here. The hypercube is now \"standing\" on a square 2-D facet, instead of a 3-D cell."
            , text " Just like for the rectangle, a slice near the \"top\" or \"bottom\" is skinny, but a slice near the middle is wide."
            ]
        3 ->
            [ text "By rotating the hypercube, you can get other interesting cross-sections, including a"
            , button 
                [onClick <| Animate <| rotateTo <| 
                    mkM4 
                        ( ( 0        , 0,  sqrt(1/2) ,-(sqrt(1/2)))
                        , ( 0        , 1, 0          , 0          )
                        , ( sqrt(2/3), 0,-(sqrt(1/6)),-(sqrt(1/6)))
                        , ( sqrt(1/3), 0,  sqrt(1/3) ,  sqrt(1/3) ))
                ] 
                [text "hexagonal prism"]
            , text " and an "
            , button 
                [onClick <| Animate <| rotateTo <| 
                    mkM4 
                        ( ( 0.5, 0.5, 0.5, 0.5)
                        , ( 0.5, 0.5,-0.5,-0.5)
                        , ( 0.5,-0.5, 0.5,-0.5)
                        , ( 0.5,-0.5,-0.5, 0.5))
                ] 
                [text "octahedron"]
            , text "."
            , br [] []
            , text "You can also rotate the hypercube yourself with the WASD QE IJKL UO keys."
            , br [] []
            , text "If you get confused, you can click \"Reset Orientation\" to rotate the hypercube to show a cube cross-section again.    "
            ]
        4 ->
            [ text "This is a simplex. It is the simplest regular polychoron (4-D polyhedron)."
            , text " It has five tetrahedral cells, ten triangular faces, ten edges, and five vertices."
            , text " Every face is adjacent to every other."
            ]
        5 ->
            [ text "There are four other regular polychora: The 16-cell, the 24-cell, the 120-cell, and the 600-cell."
            , text " (You can select them at the bottom of the screen.)"
            , br [] []
            , text " The 120-cell and 600-cell may cause lag if you do not turn off borders."
            , br [] []
            , text "Play around with them! Explore! Have fun!"
            ]
        _ -> 
            [ text "You have escaped the tutorial, into the depths of the unknown!"
            , br [] []
            , text "(a.k.a. Tutorial index out of bounds error)"
            ]


type Control 
    = Rotate
    | SetShape Shape
    | ResetOrientation

enabled : Control -> Maybe Tutorial -> Bool
enabled msg t =
    case t of
        Nothing -> True
        Just t ->
            case msg of
                Rotate -> t >= 3
                SetShape C8 -> t /= 4
                SetShape C5 -> t >= 4
                SetShape _ -> t >= 5
                ResetOrientation -> t >= 3












rotateBack : Mat4 -> Animation
rotateBack m1 =
    let (x,y,z,w) = (vec4 0 0 0 1) |> transformV4 m1 |> toTuple
        theta = atan2 x w
        n1 = ceiling (abs (50*theta))
        theta_s = theta / toFloat n1
        rotate1   = mkM4 ((cos theta  ,0,0,sin -theta  ),(0,1,0,0),(0,0,1,0),(sin theta  ,0,0,cos theta  ))
        rotate1_s = mkM4 ((cos theta_s,0,0,sin -theta_s),(0,1,0,0),(0,0,1,0),(sin theta_s,0,0,cos theta_s))
        m2 = Mat4.mul rotate1 m1
    in 
    let (x,y,z,w) = (vec4 0 0 0 1) |> transformV4 m2 |> toTuple
        theta = atan2 y w
        n2 = ceiling (abs (50*theta))
        theta_s = theta / toFloat n2
        rotate2   = mkM4 ((1,0,0,0),(0,cos theta  ,0,sin -theta  ),(0,0,1,0),(0,sin theta  ,0,cos theta  ))
        rotate2_s = mkM4 ((1,0,0,0),(0,cos theta_s,0,sin -theta_s),(0,0,1,0),(0,sin theta_s,0,cos theta_s))
        m3 = Mat4.mul rotate2 m2
    in 
    let (x,y,z,w) = (vec4 0 0 0 1) |> transformV4 m3 |> toTuple
        theta = atan2 z w
        n3 = ceiling (abs (50*theta))
        theta_s = theta / toFloat n3
        rotate3   = mkM4 ((1,0,0,0),(0,1,0,0),(0,0,cos theta  ,sin -theta  ),(0,0,sin theta  ,cos theta  ))
        rotate3_s = mkM4 ((1,0,0,0),(0,1,0,0),(0,0,cos theta_s,sin -theta_s),(0,0,sin theta_s,cos theta_s))
        m4 = Mat4.mul rotate3 m3
    in
    let (x,y,z,w) = (vec4 0 0 1 0) |> transformV4 m4 |> toTuple
        theta = atan2 x z
        n4 = ceiling (abs (50*theta))
        theta_s = theta / toFloat n4
        rotate4   = mkM4 ((cos theta  ,0,sin -theta  ,0),(0,1,0,0),(sin theta  ,0,cos theta  ,0),(0,0,0,1))
        rotate4_s = mkM4 ((cos theta_s,0,sin -theta_s,0),(0,1,0,0),(sin theta_s,0,cos theta_s,0),(0,0,0,1))
        m5 = Mat4.mul rotate4 m4
    in 
    let (x,y,z,w) = (vec4 0 0 1 0) |> transformV4 m5 |> toTuple
        theta = atan2 y z
        n5 = ceiling (abs (50*theta))
        theta_s = theta / toFloat n5
        rotate5   = mkM4 ((1,0,0,0),(0,cos theta  ,sin -theta  ,0),(0,sin theta  ,cos theta  ,0),(0,0,0,1))
        rotate5_s = mkM4 ((1,0,0,0),(0,cos theta_s,sin -theta_s,0),(0,sin theta_s,cos theta_s,0),(0,0,0,1))
        m6 = Mat4.mul rotate5 m5
    in 
    let (x,y,z,w) = (vec4 0 1 0 0) |> transformV4 m6 |> toTuple
        theta = atan2 x y
        n6 = ceiling (abs (50*theta))
        theta_s = theta / toFloat n6
        rotate6   = mkM4 ((cos theta  ,sin -theta  ,0,0),(sin theta  ,cos theta  ,0,0),(0,0,1,0),(0,0,0,1))
        rotate6_s = mkM4 ((cos theta_s,sin -theta_s,0,0),(sin theta_s,cos theta_s,0,0),(0,0,1,0),(0,0,0,1))
        m7 = Mat4.mul rotate6 m6
  
    in ((n1,rotate1_s),[(n2,rotate2_s),(n3,rotate3_s),(n4,rotate4_s),(n5,rotate5_s),(n6,rotate6_s)])

rotateTo : Mat4 -> Mat4 -> Animation
rotateTo n m =
    rotateBack 
        (Mat4.mul 
            m 
            (Mat4.inverse n |> Maybe.withDefault Mat4.identity)) -- default should never happen

append : Animation -> Animation -> Animation
append (x,xs) (y,ys) = (x,xs++(20,Mat4.identity)::y::ys)