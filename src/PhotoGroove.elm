port module PhotoGroove exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Json.Decode exposing (Decoder, bool, int, list, string, succeed, decodeString, at)
import Json.Decode.Pipeline exposing (optional, required)
import Html.Events exposing (on, onClick)
import Json.Encode as Encode
import Random
import Http
import Debug


type ThumbnailSize = Small | Medium | Large

type alias Photo =
  { url : String
  , size: Int
  , title : String
  }

photoDecoder : Decoder Photo
photoDecoder =
  succeed Photo
    |> required "url" string
    |> required "size" int
    |> optional "title" string "(untitled)"

port setFilters : FilterOptions -> Cmd msg
type alias FilterOptions =
  { url : String
  , filters: List { name : String, amount : Int}
  }

type Status
  = Loading
  | Loaded (List Photo) String
  | Errored String

type alias Model =
  { status : Status
  , chosenSize : ThumbnailSize
  , hue : Int
  , ripple : Int
  , noise : Int
  }

type Msg 
  = ClickedPhoto String
  | GotRandomPhoto Photo
  | ClickedSuppriseMe
  | ChooseSize ThumbnailSize
  | GotPhotos (Result Http.Error (List Photo))
  | SlidHue Int
  | SlidRipple Int
  | SlidNoise Int


urlPrefix : String
urlPrefix = 
  "http://elm-in-action.com/"

view : Model -> Html Msg
view model =
  div [ class "content" ] <|
    case model.status of
      Loaded photos selectedUrl ->
        viewLoaded photos selectedUrl model
      Loading ->
        []
      Errored err ->
        [ text ("Error: " ++ err) ]

viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
  [ h1 [] [ text "Photo Groove" ]
  , button
    [ onClick ClickedSuppriseMe ]
    [ text "Supprise me!"]
  , div [ class "filters" ]
      [ viewFilter SlidHue "Hue" model.hue
      , viewFilter SlidRipple "Ripple" model.ripple
      , viewFilter SlidNoise "Noise" model.noise
      ]
  , h3 [] [ text "Thumbnail Size:" ]
  , div [ id "choose-size"]
    (List.map (viewSizeChooser model.chosenSize) [Small, Medium, Large])
  , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
    (List.map (viewThumbnail selectedUrl) photos)
  , img
    [ class "large"
    , src (urlPrefix ++ "large/" ++ selectedUrl)
    ]
    []
  ]

viewThumbnail : String ->  Photo -> Html Msg
viewThumbnail selectedUrl thumb =
  img 
    [ src (urlPrefix ++ thumb.url)
    , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
    , classList [ ( "selected", selectedUrl == thumb.url ) ]
    , onClick (ClickedPhoto thumb.url)
    ]
    []

viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
  label []
    [ input
        [ type_ "radio"
        , name "size"
        , onClick (ChooseSize size)
        , checked (chosenSize == size)
        ] 
        []
    , text (sizeToString size)
    ]

sizeToString : ThumbnailSize -> String
sizeToString size =
  case size of
    Small ->
      "small"

    Medium ->
      "med"

    Large ->
      "large"

initialModel : Model
initialModel =
  { status = Loading
  , chosenSize = Medium
  , hue = 5
  , ripple = 5
  , noise = 5
  }

applyFilter : Model -> (Model, Cmd Msg)
applyFilter model =
  case model.status of
    Loaded photos selectedUrl ->
      let
        filters =
          [ { name = "Hue", amount = model.hue}
          , { name = "Ripple", amount = model.ripple}
          , { name = "Noise", amount = model.noise}
          ]

        url =
          urlPrefix ++ "large/" ++ selectedUrl

        cmd =
          setFilters { url = url, filters = filters }
      in
       ( model, Cmd.none )

    Loading ->
      ( model, Cmd.none)

    Errored _ ->
      ( model, Cmd.none)



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotRandomPhoto photo ->
      applyFilter { model | status = selectUrl photo.url model.status }

    ClickedPhoto url->
      applyFilter { model | status = selectUrl url model.status }

    ClickedSuppriseMe ->
      case model.status of
        Loaded (firstPhoto :: otherPhotos) _ ->
          ( model
          , Random.generate GotRandomPhoto
            (Random.uniform firstPhoto otherPhotos)
          )

        Loaded [] _ ->
          ( model, Cmd.none )

        Loading -> 
          ( model, Cmd.none )

        Errored _-> 
          ( model, Cmd.none )

    ChooseSize size->
      ( { model | chosenSize = size }, Cmd.none )

    GotPhotos (Ok photos) ->
      case photos of
        (first :: _) as urls ->
          ({ model | status = Loaded photos first.url}, Cmd.none )
        [] ->
          ({ model | status = Errored "No photos found"}, Cmd.none )

    GotPhotos (Err _)->
      ( { model | status = Errored "Server error!" }, Cmd.none )

    SlidHue hue ->
      ({ model | hue = hue}, Cmd.none)

    SlidRipple ripple ->
      ({ model | ripple = ripple}, Cmd.none)

    SlidNoise noise ->
      ({ model | noise = noise}, Cmd.none)

selectUrl : String -> Status -> Status
selectUrl url status =
  case status of
    Loaded photos _ ->
      Loaded photos url
    Loading ->
      status
    Errored _ ->
      status

initialCmd : Cmd Msg
initialCmd =
  list photoDecoder
    |> Http.get "http://elm-in-action.com/photos/list.json"
    |> Http.send GotPhotos

main : Program () Model Msg
main =
  Browser.element
    { init = \flags -> ( initialModel, initialCmd )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
  node "range-slider" attributes children

viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
  div [ class "filter-slider" ]
      [ label [] [ text name ]
      , rangeSlider
        [ Attr.max "11"
        , Attr.property "val" (Encode.int magnitude)
        , onSlide toMsg
        ]
        []
      , label [] [ text (String.fromInt magnitude) ]
      ]

onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
  at ["detail", "userSlidTo"] int
    |> Json.Decode.map toMsg
    |> on "slide"