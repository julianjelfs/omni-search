module App exposing (..)

import Date exposing (Date)
import Html exposing (Html, text, div, img, input)
import Html.Attributes exposing (class, src, type_, value)
import Html.Events exposing (onInput)
import OmniSearch as O

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
    , searches : List CandidateSearch
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { logo = path
    , searchText = Nothing
    , searches = []
    }
    , Cmd.none )

type Msg
    = UpdateSearchText String

extractPossibleSearches: String -> List CandidateSearch
extractPossibleSearches search =
    let
        tokens =
            O.parse search
    in
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    Maybe.map extractPossibleSearches searchText
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
