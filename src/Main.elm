module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import Browser
import Html exposing (Html, b, button, div, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Task =
    { text : String
    , completed : Bool
    }


type Filter
    = All
    | Todo
    | Completed


type alias Model =
    { filter : Filter
    , tasks : Array Task
    , inputText : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    { filter = All
    , tasks = Array.empty
    , inputText = ""
    }



-- UPDATE


type Msg
    = SetFilter Filter
    | AddTask String
    | CompleteTask Int
    | RemoveTask Int
    | UpdateInputText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFilter filter ->
            ( { model
                | filter = filter
              }
            , Cmd.none
            )

        AddTask text ->
            ( { model
                | tasks = Array.push { text = text, completed = False } model.tasks
                , inputText = ""
              }
            , Cmd.none
            )

        CompleteTask index ->
            ( { model
                | tasks = Array.Extra.update index (\task -> { text = task.text, completed = True }) model.tasks
              }
            , Cmd.none
            )

        RemoveTask index ->
            ( { model
                | tasks = Array.Extra.removeAt index model.tasks
              }
            , Cmd.none
            )

        UpdateInputText text ->
            ( { model
                | inputText = text
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ filterSelector model.filter
        , taskList model.filter model.tasks
        , addTask model.inputText
        ]


filterName : Filter -> String
filterName filter =
    case filter of
        All ->
            "?????????"

        Todo ->
            "?????????"

        Completed ->
            "??????"


filterSelector : Filter -> Html Msg
filterSelector currentFilter =
    let
        filterButton : Filter -> Html Msg
        filterButton filter =
            if currentFilter /= filter then
                button [ onClick <| SetFilter filter ] [ text <| filterName filter ]

            else
                b [] [ text <| filterName filter ]
    in
    div []
        [ text "??????????????????"
        , filterButton All
        , filterButton Todo
        , filterButton Completed
        ]


taskToString : Task -> String
taskToString task =
    task.text
        ++ (if task.completed then
                " (??????)"

            else
                " (?????????)"
           )


taskList : Filter -> Array Task -> Html Msg
taskList filter tasks =
    let
        predicate : Task -> Bool
        predicate task =
            case filter of
                All ->
                    True

                Todo ->
                    not task.completed

                Completed ->
                    task.completed

        makeElement : Int -> Task -> Html Msg
        makeElement index task =
            li []
                [ button [ onClick <| CompleteTask index ] [ text "??????" ]
                , button [ onClick <| RemoveTask index ] [ text "??????" ]
                , text <| taskToString task
                ]

        taskElements =
            tasks
                |> Array.filter predicate
                |> Array.indexedMap makeElement
                |> Array.toList
    in
    ul [] taskElements


addTask : String -> Html Msg
addTask inputText =
    div []
        [ input [ value inputText, onInput UpdateInputText ] []
        , button [ onClick <| AddTask inputText ] [ text "??????" ]
        ]
