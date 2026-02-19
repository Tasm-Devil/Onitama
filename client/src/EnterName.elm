module EnterName exposing (Model(..), Msg(..), getCardSet, getName, update, view)

import Api exposing (CardSet(..))
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onCheck, onInput, onSubmit)


type Model
    = Entering String CardSet
    | Joining String CardSet
    | JoinError String String CardSet


getName : Model -> Maybe String
getName model =
    case model of
        Entering name _ ->
            Just name

        JoinError name _ _ ->
            Just name

        Joining _ _ ->
            Nothing


getCardSet : Model -> CardSet
getCardSet model =
    case model of
        Entering _ cardSet ->
            cardSet

        JoinError _ _ cardSet ->
            cardSet

        Joining _ cardSet ->
            cardSet


type Msg
    = TypingName String
    | ToggleExpansion Bool
    | RequestJoin


update : Msg -> Model -> Model
update msg model =
    case msg of
        TypingName newname ->
            case model of
                Entering _ cardSet ->
                    Entering newname cardSet

                JoinError _ error cardSet ->
                    JoinError newname error cardSet

                Joining _ _ ->
                    model

        ToggleExpansion checked ->
            let
                cardSet =
                    if checked then
                        WithExpansion

                    else
                        BaseOnly
            in
            case model of
                Entering name _ ->
                    Entering name cardSet

                JoinError name error _ ->
                    JoinError name error cardSet

                Joining _ _ ->
                    model

        RequestJoin ->
            model


view : Bool -> Model -> List String -> List (Html Msg)
view showCardSetOption model playerNames =
    case model of
        Entering currentName cardSet ->
            [ Html.form [ HtmlA.id "name-form", onSubmit RequestJoin ]
                [ Html.h1 []
                    [ Html.text "Onitama" ]
                , Html.small [] [ Html.text "Enter your name or select from existing players..." ]
                , viewNameInput currentName "Join" playerNames
                , if showCardSetOption then
                    viewCardSetToggle cardSet

                  else
                    Html.text ""
                ]
            ]

        Joining playerName _ ->
            [ Html.h2 [] [ Html.text "Joining game..." ]
            , Html.p [] [ Html.text ("Joining as " ++ playerName) ]
            , Html.div [ HtmlA.class "spinner" ] []
            ]

        JoinError playerName errorMsg cardSet ->
            [ Html.form [ HtmlA.id "name-form", onSubmit RequestJoin ]
                [ Html.h1 [] [ Html.text "Onitama" ]
                , Html.div [ HtmlA.class "error-message" ]
                    [ Html.text errorMsg ]
                , viewNameInput playerName "Try Again" playerNames
                , if showCardSetOption then
                    viewCardSetToggle cardSet

                  else
                    Html.text ""
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


viewCardSetToggle : CardSet -> Html Msg
viewCardSetToggle cardSet =
    Html.label [ HtmlA.class "card-set-toggle" ]
        [ Html.input
            [ HtmlA.type_ "checkbox"
            , HtmlA.checked (cardSet == WithExpansion)
            , onCheck ToggleExpansion
            ]
            []
        , Html.text " Include Sensei's Path expansion cards"
        ]
