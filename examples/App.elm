module App exposing (..)

import Date exposing (Date)
import Html exposing (Html, div, img, input, text, textarea)
import Html.Attributes exposing (class, cols, placeholder, rows, src, style, type_, value)
import Html.Events exposing (onInput)
import OmniSearch as O
import Task


type alias Model =
    { searchText : Maybe String
    , searches : List O.SearchToken
    , now : Maybe Date.Date
    }


init : ( Model, Cmd Msg )
init =
    ( { searchText = Nothing
      , searches = []
      , now = Nothing
      }
    , Task.perform Now Date.now
    )


type Msg
    = UpdateSearchText String
    | Now Date.Date


extractPossibleSearches : Date.Date -> String -> List O.SearchToken
extractPossibleSearches now search =
    O.parse now search
        |> List.filter
            (\s ->
                case s of
                    O.Other _ ->
                        False

                    _ ->
                        True
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Now d ->
            ( { model | now = Just d }
            , Cmd.none
            )

        UpdateSearchText s ->
            let
                searchText =
                    if s == "" then
                        Nothing
                    else
                        Just s
            in
                ( { model
                    | searchText = searchText
                    , searches =
                        Maybe.map2 extractPossibleSearches model.now searchText
                            |> Maybe.withDefault []
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    div [ style
            [("padding", "40px")]
        ]
        [ textarea
            [ style [("width", "500px")]
            , rows 10
            , cols 200
            , placeholder "Enter a search e.g. \"hotel in tenerife 2 rooms 2 adults 18/09/2017 1 week\""
            , onInput UpdateSearchText
            ]
            [ text <| Maybe.withDefault "" model.searchText ]
        , showSearches model
        ]


showSearches : Model -> Html Msg
showSearches model =
    div
        [ style
            [ ("margin-top", "10px")
            , ("font-weight", "bold")
            , ("font-size", "14px")
            ]
        ]
        (List.map
            (\s ->
                div
                    []
                    [ text <| toString s ]
            )
            model.searches
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
