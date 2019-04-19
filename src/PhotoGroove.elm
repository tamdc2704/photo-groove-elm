module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Http

type ThumbnailSize = Small | Medium | Large

type alias Photo =
  { url : String }

type Status
  = Loading
  | Loaded (List Photo) String
  | Errored String

type alias Model =
  { status : Status
  , chosenSize : ThumbnailSize
  }

type Msg 
  = ClickedPhoto String
  | GotRandomPhoto Photo
  | ClickedSuppriseMe
  | ChooseSize ThumbnailSize
  | GotPhotos (Result Http.Error String)


urlPrefix : String
urlPrefix = 
  "http://elm-in-action.com/"

view : Model -> Html Msg
view model =
  div [ class "content" ] <|
    case model.status of
      Loaded photos selectedUrl ->
        viewLoaded photos selectedUrl model.chosenSize
      Loading ->
        []
      Errored err ->
        [ text ("Error: " ++ err) ]

viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
  [ h1 [] [ text "Photo Groove" ]
  , button 
    [ onClick ClickedSuppriseMe ] 
    [ text "Supprise me!"]
  , h3 [] [ text "Thumbnail Size:" ]
  , div [ id "choose-size"]
    (List.map (viewSizeChooser chosenSize) [Small, Medium, Large])
  , div [ id "thumbnails", class (sizeToString chosenSize) ]
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
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotRandomPhoto photo ->
      ( { model | status = selectUrl photo.url model.status}, Cmd.none )

    ClickedPhoto url->
      ( { model | status = selectUrl url model.status}, Cmd.none )

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

    GotPhotos (Ok resStr) -> 
      case String.split "," resStr of
        (firstUrl :: _) as urls ->
          let     
            photos =
              List.map Photo urls
          in
            ({ model | status = Loaded photos firstUrl}, Cmd.none )
        [] ->
          ({ model | status = Errored "No photos found"}, Cmd.none )
                
    GotPhotos (Err _)->
              ( { model | status = Errored "Server error!" }, Cmd.none )

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
  Http.get
    { url = "http://elm-in-action.com/photos/list"
    , expect = Http.expectString GotPhotos
    }

main : Program () Model Msg
main =
  Browser.element
    { init = \flags -> ( initialModel, initialCmd )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }