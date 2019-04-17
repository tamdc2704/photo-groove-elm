module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type ThumbnailSize = Small | Medium | Large

type alias Photo =
  { url : String }

type alias Model =
  { photos : List Photo
  , selectedUrl : String
  , chosenSize : ThumbnailSize
  }

type Msg 
  = ClickedPhoto String
  | ClickedSuppriseMe 
  | ChooseSize ThumbnailSize


urlPrefix : String
urlPrefix = 
  "http://elm-in-action.com/"

view : Model -> Html Msg
view model =
  div [ class "content" ] 
    [ h1 [] [ text "Photo Groove" ]
    , button 
      [ onClick ClickedSuppriseMe ] 
      [ text "Supprise me!"]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size"]
      (List.map (viewSizeChooser model.chosenSize) [Small, Medium, Large])
    , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
      (List.map (viewThumbnail model.selectedUrl) model.photos)
    , img
      [ class "large"
      , src (urlPrefix ++ model.selectedUrl)
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
viewSizeChooser choosenSize size =
  label []
    [ input 
        [ type_ "radio"
        , name "size" 
        , onClick (ChooseSize size)
        , checked (choosenSize == size)
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
  { photos =
      [ {url = "1.jpeg"}
      , {url = "2.jpeg"}
      , {url = "3.jpeg"}
      ]
  , selectedUrl = "1.jpeg"
  , chosenSize = Medium
  }

photoArray : Array Photo
photoArray = Array.fromList initialModel.photos

getPhotoUrl : Int -> String
getPhotoUrl index =
  case Array.get index photoArray of  
    Just photo ->
      photo.url

    Nothing ->
      ""


update : Msg -> Model -> Model
update msg model =
  case msg of
    ClickedPhoto url->
      { model | selectedUrl = url }

    ClickedSuppriseMe ->
      { model | selectedUrl = "2.jpeg"}

    ChooseSize size->
      { model | chosenSize = size }

main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }