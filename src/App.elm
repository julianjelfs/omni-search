module App exposing (..)

import Date exposing (Date)
import Html exposing (Html, text, div, img, input)
import Html.Attributes exposing (class, src, type_, value)
import Html.Events exposing (onInput)
import OmniSearch as O
import Task

type CandidateSearch
    = Hotel HotelSearch

type alias HotelSearch =
    { checkIn: Maybe Date
    , checkOut: Maybe Date
    , adults : Int
    , childAges : List Int
    , destination : Destination
    }

type alias Destination =
    { type_ : Int
    , id : Int
    , title : String
    }

type alias Model =
    { logo : String
    , searchText : Maybe String
    , searches : List O.SearchToken
    , now : Maybe Date.Date
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { logo = path
    , searchText = Nothing
    , searches = []
    , now = Nothing
    }
    , Task.perform Now Date.now )

type Msg
    = UpdateSearchText String
    | Now Date.Date

extractPossibleSearches: Date.Date -> String -> List O.SearchToken
extractPossibleSearches now search =
    O.parse now search []
        |> List.filter
            (\s ->
                case s of
                    O.Other _ -> False
                    _ -> True)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Now d ->
            ( { model | now = Just d }
            , Cmd.none )

        UpdateSearchText s ->
            let
                searchText =
                    if s == "" then
                        Nothing
                    else
                        Just s
            in
            ( { model | searchText = searchText
             , searches =
                    Maybe.map2 extractPossibleSearches model.now searchText
                        |> Maybe.withDefault []
            }
            , Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ img [ src model.logo ] []
        , div []
            [ input
                [ type_ "text"
                , class "search-box"
                , value <| Maybe.withDefault "" model.searchText
                , onInput UpdateSearchText ]
                []
            , showSearches model
            ]
        ]

showSearches : Model -> Html Msg
showSearches model =
    div
        [class "searches"]
        (List.map
            (\s ->
                div
                    []
                    [ text <| toString s ]
            )
            model.searches)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
