module Main exposing (main)

import Api exposing (Msg(..))
import Browser
import Browser.Navigation as Nav exposing (Key)
import Game.Figure exposing (Color(..))
import Game.Game as Game exposing (Game, GameMove, GameState(..), Msg(..))
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onClick, onInput)
import Lobby exposing (GameId, Model, Msg(..), Status(..))
import Url exposing (Url)



-- TYPES


type alias PlayerName =
    String



-- MODEL


type Model
    = Redirect Url Key
    | Lobby Lobby.Model
    | EnterName PlayerName GameId Key
    | Playing PlayerName GameId Game (List Game.GameMove) Key


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( Redirect url key, Cmd.map GotServerMsg Api.getGamesIdsFromServer )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Onitama"
        [ case model of
            Redirect url _ ->
                Html.div [ HtmlA.class "lobby" ]
                    [ Html.h1 []
                        [ Html.text url.path ]
                    ]

            Lobby m ->
                Lobby.view m
                    |> Html.map GotLobbyMsg

            EnterName player _ _ ->
                Html.div [ HtmlA.class "landing-screen" ]
                    --, HtmlA.style "display" "none" ]
                    [ Html.form [ HtmlA.id "name-form" ]
                        [ Html.h1 []
                            [ Html.text "Onitama" ]
                        , Html.small [] [ Html.text "Enter your name..." ]
                        , Html.div [ HtmlA.class "name-line" ]
                            [ Html.input [ HtmlA.id "name", HtmlA.placeholder "Enter your name", HtmlA.value <| player, onInput TypingName ] []
                            , Html.input [ HtmlA.type_ "button", HtmlA.value "Join", onClick RequestGameFromServer ] []
                            ]

                        {-
                           , Html.div [ HtmlA.class "rejoin", HtmlA.attribute "style" "display: block" ]
                               [ Html.span [] [ Html.text "You appear to have a rejoin code for this room. Click the button below if you want to rejoin as the player you were previously.          " ]
                               , Html.button [ HtmlA.id "rejoin-button", HtmlA.type_ "button" ] [ Html.text "Rejoin" ]
                               ]
                        -}
                        ]
                    ]

            Playing _ _ game history _ ->
                Html.div [ HtmlA.class "game-container", HtmlA.style "display" "flex" ]
                    ((game
                        |> Game.view
                        |> List.map (Html.map GotGameMsg)
                     )
                        ++ [ Html.div []
                                [ Html.input [ HtmlA.type_ "button", HtmlA.value "Update", onClick RequestGameFromServer ] []
                                , viewHistory history
                                ]
                           ]
                    )
        ]



{-

   viewHistoryOrError : Model -> Html Msg
   viewHistoryOrError model =
       --    case model.errorMessage of
       --        Just message ->
       --            viewError message
       --        Nothing ->
       case model of
           Playing _ _ _ history _ ->
               viewHistory history

           _ ->
               Html.text "Das sollten Sie nicht sehen"

      viewError : String -> Html Msg
      viewError errorMessage =
          let
              errorHeading =
                  "Couldn't fetch data at this time."
          in
          Html.div []
              [ Html.h3 [] [ Html.text errorHeading ]
              , Html.text ("Error: " ++ errorMessage)
              ]
-}


viewHistory : List GameMove -> Html Msg
viewHistory history =
    Html.div [ HtmlA.class "game-log" ]
        [ Html.ul [ HtmlA.id "log-lines" ]
            (List.map viewGameMove <| List.reverse history)
        , Html.input [ HtmlA.id "chat-box", HtmlA.type_ "text" ] []
        ]


viewGameMove : GameMove -> Html Msg
viewGameMove gameMove =
    let
        ( from_x, from_y ) =
            ( 1 + Tuple.first gameMove.from, 1 + Tuple.second gameMove.from )

        ( move_x, move_y ) =
            gameMove.move

        ( to_x, to_y ) =
            ( from_x + move_x, from_y + move_y )

        ( from_x_char, to_x_char ) =
            ( Char.fromCode (96 + from_x), Char.fromCode (96 + to_x) )

        from =
            String.fromChar from_x_char ++ String.fromInt from_y

        to =
            String.fromChar to_x_char ++ String.fromInt to_y
    in
    Html.li [ HtmlA.class "log-message" ]
        [ Html.text (Game.Figure.colorToString gameMove.color ++ " moved from " ++ from ++ " to " ++ to ++ " by playing the " ++ gameMove.card.name ++ " card.") ]



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotGameMsg Game.Msg
    | GotLobbyMsg Lobby.Msg
    | TypingName String -- EnterName
    | RequestGameFromServer -- EnterName
    | GotServerMsg Api.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl url, Lobby lobby ) ->
            let
                gameid =
                    String.dropLeft 1 url.path
            in
            case lobby.status of
                Home gamesids ->
                    if List.member gameid gamesids then
                        -- this happens only on url change
                        ( EnterName "Wendy" gameid lobby.key, Cmd.none )

                    else
                        ( model, Cmd.none )

        ( ChangedUrl url, EnterName _ _ key ) ->
            ( Redirect url key, Cmd.map GotServerMsg Api.getGamesIdsFromServer )

        ( ChangedUrl url, Playing _ _ _ _ key ) ->
            ( Redirect url key, Cmd.map GotServerMsg Api.getGamesIdsFromServer )

        ( ClickedLink urlRequest, Lobby lobby ) ->
            -- user clicked on a Join link in the table
            case urlRequest of
                Browser.Internal url ->
                    ( Lobby lobby, Nav.pushUrl lobby.key <| Url.toString url )

                _ ->
                    ( model, Cmd.none )

        ( GotGameMsg gamemsg, Playing name gameid game history_ key ) ->
            -- game signals a new gamemove to this update function by MoveDone
            -- let the server know by invoking postNewGameMove
            let
                game_after =
                    Game.update gamemsg game
            in
            ( Playing name gameid game_after history_ key
            , case game_after.state of
                MoveDone gameMove ->
                    transformGameMove gameMove
                        |> Api.postNewGameMove gameid
                        |> Cmd.map GotServerMsg

                _ ->
                    Cmd.none
            )

        ( GotLobbyMsg RequestNewGameFromServer, Lobby _ ) ->
            -- User clicked on 'New Game' button
            ( model, Cmd.map GotServerMsg Api.getGameIdFromServer )

        ( TypingName newname, EnterName _ gameid key ) ->
            ( EnterName newname gameid key, Cmd.none )

        ( RequestGameFromServer, EnterName name gameid _ ) ->
            -- User completed entering name and clicked the join button
            ( model, Cmd.map GotServerMsg <| Api.joinGame gameid name )

        ( RequestGameFromServer, Playing _ gameid _ _ _ ) ->
            -- fetch latest gamemove from server
            ( model, Cmd.map GotServerMsg <| Api.getGameFromServer gameid )

        ( GotServerMsg (ReceivedGameIdsFromServer (Ok gamesids)), Redirect url key ) ->
            -- all the games succesfully fetched from server
            let
                gameid =
                    String.dropLeft 1 url.path
            in
            if List.member gameid gamesids then
                -- enter a game directly by URL and not by pressing the new game button
                ( EnterName "Bob" gameid key, Cmd.none )

            else if String.isEmpty gameid then
                -- the default way to enter the lobby
                ( Lobby { status = Home gamesids, key = key }, Cmd.none )

            else
                -- The URL points to an nonexisting gameid
                ( Lobby { status = Home gamesids, key = key }, Nav.pushUrl key <| "/" )

        ( GotServerMsg (ReceivedGameIdsFromServer (Err _)), _ ) ->
            Debug.todo "branch 'ReceivedGameIdsFromServer (Err httpError)' not implemented"

        ( GotServerMsg (ReceivedGameIdFromServer (Ok gameId)), Lobby lobby ) ->
            -- getGameIdFromServer was succesfull. Created a new game on the server.
            -- Append new game id to the list, change the URL and let ( ChangedUrl url , Lobby lobby) handle it.
            case lobby.status of
                Home gamesids ->
                    ( Lobby { lobby | status = Home (gameId :: gamesids) }, Nav.pushUrl lobby.key <| "/" ++ gameId )

        ( GotServerMsg (ReceivedGameFromServer (Ok servergame)), EnterName name gameid key ) ->
            -- joinGame was succesfull. Now create a new game in Browser
            let
                newgame =
                    Game.setupNewGame servergame.cards
                        (if name == servergame.player_black then
                            Black

                         else
                            White
                        )
                        White

                -- ToDo: White is not always the first player!
                finalgame =
                    List.foldr (\gameMove -> Game.update (NewGameMove <| transformGameMove gameMove)) newgame servergame.history
            in
            ( Playing name gameid finalgame servergame.history key, Cmd.none )

        ( GotServerMsg (ReceivedGameFromServer (Ok servergame)), Playing name gameid game _ key ) ->
            -- getGameFromServer was succesfull. Append the last gamemove from opponent to the history.
            List.head servergame.history
                |> Maybe.map
                    (\gameMove ->
                        ( Playing name gameid (game |> Game.update (NewGameMove <| transformGameMove gameMove)) servergame.history key
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        ( GotServerMsg (ReceivedPostCreatedFromServer (Ok gameMove)), Playing name gameid game history_ key ) ->
            ( Playing name gameid (game |> Game.update (NewGameMove <| transformGameMove gameMove)) (gameMove :: history_) key
            , Cmd.none
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


transformGameMove : Game.GameMove -> Game.GameMove
transformGameMove g =
    case g.color of
        Black ->
            Game.rotateGameMove g

        White ->
            g



{-
   buildErrorMessage : Http.Error -> String
   buildErrorMessage httpError =
       case httpError of
           Http.BadUrl message ->
               message

           Http.Timeout ->
               "Server is taking too long to respond. Please try again later."

           Http.NetworkError ->
               "Unable to reach server."

           Http.BadStatus statusCode ->
               "Request failed with status code: " ++ String.fromInt statusCode

           Http.BadBody message ->
               message
-}
-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
