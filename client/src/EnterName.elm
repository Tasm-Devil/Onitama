module EnterName exposing (Model(..), Msg(..), getName, update, view)

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onClick, onInput, onSubmit)


type Model
    = Entering String
    | JoinError String String


getName : Model -> Maybe String
getName model =
    case model of
        Entering name ->
            Just name

        JoinError name _ ->
            Just name


type Msg
    = TypingName String
    | RequestJoin
    | Cancel


update : Msg -> Model -> Model
update msg model =
    case msg of
        TypingName newname ->
            case model of
                Entering _ ->
                    Entering newname

                JoinError _ error ->
                    JoinError newname error

        RequestJoin ->
            model

        Cancel ->
            model


view : Model -> List String -> List (Html Msg)
view model playerNames =
    case model of
        Entering currentName ->
            [ Html.form [ HtmlA.id "name-form", onSubmit RequestJoin ]
                [ Html.h1 []
                    [ Html.text "Onitama" ]
                , Html.small [] [ Html.text "Enter your name to continue..." ]
                , viewNameInput currentName "Continue" playerNames
                , Html.button [ HtmlA.class "cancel-button", HtmlA.type_ "button", onClick Cancel ]
                    [ Html.text "Cancel" ]
                , privacyNotice
                ]
            ]

        JoinError playerName errorMsg ->
            [ Html.form [ HtmlA.id "name-form", onSubmit RequestJoin ]
                [ Html.h1 [] [ Html.text "Onitama" ]
                , Html.div [ HtmlA.class "error-message" ]
                    [ Html.text errorMsg ]
                , viewNameInput playerName "Try Again" playerNames
                , Html.button [ HtmlA.class "cancel-button", HtmlA.type_ "button", onClick Cancel ]
                    [ Html.text "Cancel" ]
                , privacyNotice
                ]
            ]


privacyNotice : Html Msg
privacyNotice =
    Html.small [ HtmlA.class "privacy-notice" ]
        [ Html.text "Your player name and a session token are stored on a server in Falkenstein, Germany to identify you during the game. No other personal data is collected." ]


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
