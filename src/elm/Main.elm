module Main exposing (..)

import Date exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (..)
import Json.Decode as Json exposing (..)
import Json.Decode.Extra exposing (..)
import Task exposing (Task)
import Http exposing (..)
import Date.Format exposing (format)

type alias Meetup = 
  { date     : Date
  , location : String
  , people   : Maybe Int
  , topics   : String
  }

type Resource data 
  = Loading 
  | Loaded data
  | Error Http.Error

type alias Model = 
  { next     : Resource (Maybe Meetup)
  , previous : Resource (Maybe Meetup)
  , future   : Resource (Maybe (List Meetup))
  , meetups  : Maybe (List Meetup)
  }

init : ( Model, Cmd Msg )
init = 
  ({ next     = Loading
   , previous = Loading
   , future   = Loading
   , meetups  = Nothing
   }
   , fetchMeetups)

type Msg 
  = Fetch
  | FetchResult (Result Http.Error (List Meetup))
  | SetDate Date

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Fetch                 -> (model, fetchMeetups)
    SetDate date          -> (partitionData date model, Cmd.none)
    FetchResult (Ok data) -> ({ model | meetups = Just data}, getDate)
    FetchResult (Err msg) -> ({ model
                              | next     = Error msg
                              , previous = Error msg
                              , future   = Error msg
                              }, Cmd.none)

partitionData : Date -> Model -> Model
partitionData date model =
  case model.meetups of
    Just (meetups) -> 
      let (old, upcoming) = partition (\ m -> (Date.toTime m.date) < (Date.toTime date)) meetups
      in
        { model 
        | next     = Loaded (head upcoming)
        , previous = Loaded (head <| reverse old)
        , future   = Loaded (tail upcoming)
        }
    Nothing       -> model

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model = 
  div [class "main"] [ div [ class "header" ] 
            [ div [ class "flex-container" ] [ div [ class "flex-fill" ] [ h1 [] [text "Tech & Wings"] ]
                                                  , div [] [ a [href "slack.htm"] [ i [class "fa fa-1x fa-slack"] []
                                                                                  , text "join us on slack"  ] ]]
            , h2 [] [text "A group of developers, IT specialiasts, and enthusiasts gathering together to talk all kinds of tech related topics." ]
            ]
         , div [class "next-meetup"]
               [ h1 [] [text "Next Meetup"]
               , div [class "container"] [viewNextMeetup model.next]
               ]
         , div [class "previous-meetup"] 
                [ h2 [] [text "Previous Meetup"]
                , div [class "container"] [viewPreviousMeetup model.previous]
                ]
         , div [class "future-meetups"]
                [ h2 [] [text "Future Meetups"]
                , div [class "container"] [viewFutureMeetups model.future]
                ]
         , viewFooter 
         ]

viewFooter : Html Msg
viewFooter = 
  div [class "footer"] 
       [ p [] [ text "Created with "
               , a [href "http://elm-lang.org"] [ text "Elm"]]
       , p [] [ text "Source code found on "
              , a [href "https://github.com/shanecharles/techandwings_ui.git"] [text "GitHub"]]
       ]

loadingSpinner = i [ class "fa fa-cog fa-3x fa-spin center"
                   , attribute "aria-hidden" "true" ] []

formatDateString : Date -> String
formatDateString = format "%A, %B %e, %Y"

formatTimeString : Date -> String
formatTimeString = format "%l:%M %p"

dataLine : String -> String -> Html Msg
dataLine label data =
  div [class "line"] [ div [class "label"] [text label]
                     , div [class "data"] [text data]]

viewNextMeetup : Resource (Maybe Meetup) -> Html Msg
viewNextMeetup model =
  case model of
    Loading           -> div [] [loadingSpinner]
    Loaded (Nothing)  -> div [] [text "There is no meetup scheduled yet. Check again later."]
    Loaded (Just (m)) -> div [] 
                          [ div [class "logo"] []
                          , div [class "next-details"] 
                            [ dataLine "When" (formatDateString m.date)
                            , dataLine "Time" (formatTimeString m.date)
                            , dataLine "Where" m.location
                          ]]
    Error msg         -> div [] [ h1 [] [text (":( " ++ (msg |> toString))]]

viewFutureMeetups: Resource (Maybe (List Meetup)) -> Html Msg
viewFutureMeetups model =
  let noMeetupsMsg = "No meetups planned at the moment... check back later." in
  case model of
    Loading             -> div [] [loadingSpinner]
    Loaded (Nothing)    -> div [] [text noMeetupsMsg]
    Loaded (Just ([]))  -> div [] [text noMeetupsMsg]
    Loaded (Just (mts)) -> div [] (mts |> List.map (\ m -> dataLine "When" (m.date |> formatDateString)))
    Error msg           -> div [] [text "uh oh... someone talk with the dev!"]

formatPeople : Maybe Int -> String 
formatPeople people =
  case people of 
    Just (p) -> p |> toString
    Nothing  -> "¯\\_(ツ)_/¯"

viewPreviousMeetup : Resource (Maybe Meetup) -> Html Msg
viewPreviousMeetup model =
  case model of
    Loading           -> div [] [loadingSpinner]
    Loaded (Nothing)  -> div [] [text "Was there really a previous meeting?"]
    Loaded (Just (m)) -> div [] [ dataLine "When" (m.date |> formatDateString) 
                                , dataLine "Where" m.location
                                , dataLine "People" (m.people |> formatPeople)
                                , dataLine "Topics" m.topics
                                ]    
    Error msg         -> div [] [ text "uh oh... someone talk with the dev!"]

--dataUrl = "http://localhost:8083/meetups"
--dataUrl = "https://techandwingsfunctions.azurewebsites.net/api/meetups"
dataUrl = "https://techandwings.ca/data/meetups.min.json"

meetupDecoder : Decoder Meetup
meetupDecoder = Json.map4 Meetup
                     (field "date" date)
                     (field "location" string)
                     (maybe (field "people" int))
                     (field "topics" string)

meetupsDecoder : Decoder (List Meetup)
meetupsDecoder = Json.list <| meetupDecoder

fetchMeetups : Cmd Msg
fetchMeetups =
  let request = Http.get dataUrl meetupsDecoder
  in
    Http.send FetchResult request

getDate : Cmd Msg
getDate = Date.now
          |> Task.perform SetDate

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
