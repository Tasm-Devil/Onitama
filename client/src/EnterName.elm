module EnterName exposing (Model(..), Msg(..), getName, update, view)

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onInput, onSubmit)


type Model
    = Entering String
    | Joining String
    | JoinError String String


getName : Model -> Maybe String
getName model =
    case model of
        Entering name ->
            Just name

        JoinError name _ ->
            Just name

        Joining _ ->
            Nothing


type Msg
    = TypingName String
    | RequestJoin


update : Msg -> Model -> Model
update msg model =
    case msg of
        TypingName newname ->
            case model of
                Entering _ ->
                    Entering newname

                JoinError _ error ->
                    JoinError newname error

                Joining _ ->
                    model

        RequestJoin ->
            model


view : Model -> List String -> List (Html Msg)
view model playerNames =
    case model of
        Entering currentName ->
            [ Html.form [ HtmlA.id "name-form", onSubmit RequestJoin ]
                [ Html.h1 []
                    [ Html.text "Onitama" ]
                , Html.small [] [ Html.text "Enter your name or select from existing players..." ]
                , viewNameInput currentName "Join" playerNames
                ]
            ]

        Joining playerName ->
            [ Html.h2 [] [ Html.text "Joining game..." ]
            , Html.p [] [ Html.text ("Joining as " ++ playerName) ]
            , Html.div [ HtmlA.class "spinner" ] []
            ]

        JoinError playerName errorMsg ->
            [ Html.form [ HtmlA.id "name-form", onSubmit RequestJoin ]
                [ Html.h1 [] [ Html.text "Onitama" ]
                , Html.div [ HtmlA.class "error-message" ]
                    [ Html.text errorMsg ]
                , viewNameInput playerName "Try Again" playerNames
                ]
            ]


viewNameInput : String -> String -> List String -> Html Msg
viewNameInput name submitLabel playerNames =
    Html.div [ HtmlA.class "name-line" ]
        [ Html.input
            [ HtmlA.id "name"
            , HtmlA.placeholder "Enter your name"
            , HtmlA.value name
            , HtmlA.attribute "autocomplete" "off"
            , HtmlA.attribute "list" "player-names"
            , onInput TypingName
            ]
            []
        , Html.datalist [ HtmlA.id "player-names" ]
            (List.map (\p -> Html.option [ HtmlA.value p ] []) playerNames)
        , Html.input
            [ HtmlA.type_ "submit"
            , HtmlA.value submitLabel
            , HtmlA.disabled (String.isEmpty name)
            ]
            []
        ]
