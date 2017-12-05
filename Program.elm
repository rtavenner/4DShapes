module Program exposing (Program, program)

import Window
import Task
import Html

type alias Program model msg = 
    Platform.Program Never (Maybe (Window.Size, model)) (Result Window.Size msg)

program : 
    { init : model
    , update : msg -> model -> model
    , subscriptions : model -> Sub msg
    , view : Window.Size -> model -> Html.Html msg 
    } -> Program model msg
program prog =
    Html.program
        { init = (Nothing, Task.perform Err Window.size)
        , update = 
            \msg model -> 
                case (msg, model) of
                    (Err size, Nothing) -> (Just (size, prog.init), Cmd.none)
                    (Err size, Just (_, model)) -> (Just (size, model), Cmd.none)
                    (Ok msg, Nothing) -> (Nothing, Cmd.none)
                    (Ok msg, Just (size, model)) -> (Just (size, prog.update msg model), Cmd.none)
        , subscriptions = 
            \model -> 
                case model of
                    Nothing ->
                        Window.resizes Err
                    Just (_,model) ->
                        Sub.batch
                            [ Window.resizes Err
                            , Sub.map Ok (prog.subscriptions model)
                            ]
        , view = 
            \model ->
                case model of 
                    Nothing -> Html.text "Loading"
                    Just (size, model) -> 
                        Html.map Ok (prog.view size model)
        }