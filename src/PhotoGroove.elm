port module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, id, name, src, title, type_)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type ThumbnailSize
    = Small
    | Medium
    | Large


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize -- A container that holds a `ThumbnailSize` value.
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"

            -- Elm applies special handling to the `value` property. So we name it `val` instead.
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ class "filters" ]
        {- `viewFilter` takes in an `(Int -> Msg)` as first parameter. Why can we just pass `SlidX`?
           Remember that when we create a type variant, it comes automatically with a function that returns our `Msg`
           type.

           This then allows us to pass that `(Int -> Msg)` type down to our `onSlide` function which uses type variable
           `msg` to return an `Attribute msg`. So in a sense, passing down `(Int -> Msg)`, our `onSlide` function will
           return actually a `Attribute Msg`.

           When we slide one of these, JavaScript gets triggered --> our `onSlide` function listens for the "slide"
           event --> internally converts the slider integer value into the `Msg` passed in --> our view dispatches the
           message to our `update` function --> `update` function updates the model and the view gets reflected with it.
        -}
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , canvas [ id "main-canvas", class "large" ] []
    ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")

        {- classList has list of tuples, with each tuple containing the desired class name and then the Boolean check
           indicate whether we want the class to be added to the final class string.
        -}
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
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



{- We don't need to implement this function. The `port` keyword  will look at the type we requested and decide what the
   function will do. In our case, we send `FilterOptions` value that we pass to this.

   - The `port` keyword must be followed by a function name and type annotation.
   - The type annotation must be for a function that takes one argument.
   - The function must return `Cmd msg`, and nothing else. Not even `Cmd Msg` (our custom type).

   Since `Cmd msg` is like what `Cmd.none` returns, we actually won't get a message back.
-}


port setFilters : FilterOptions -> Cmd msg


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Int }
    }


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


photoDecoder : Decoder Photo
photoDecoder =
    -- If decoding succeeds, pass these values to `buildPhoto`.
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


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


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                {- `firstPhoto : Photo` and `otherPhotos : List Photo`.
                   This branch represents when we got at least one photo.
                -}
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                {- This branch matches when we have an empty list. Elm will cry out loud if you forgot to handle this
                   branch
                -}
                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored errorMessage ->
                    ( model, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        GotPhotos (Ok photos) ->
            case photos of
                first :: rest ->
                    ( { model | status = Loaded photos first.url }
                    , Cmd.none
                    )

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err httpError) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        SlidHue hue ->
            ( { model | hue = hue }, Cmd.none )

        SlidRipple ripple ->
            ( { model | ripple = ripple }, Cmd.none )

        SlidNoise noise ->
            ( { model | noise = noise }, Cmd.none )


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = model.hue }
                    , { name = "Ripple", amount = model.ripple }
                    , { name = "Noise", amount = model.noise }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored errorMessage ->
            ( model, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored errorMessage ->
            status


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }



{- The type `Program () Model Msg` indicates an Elm `Program` that has no `flags`, whose model type is `Model` and
   whose message type is `Msg`.
-}


main : Program () Model Msg
main =
    Browser.element
        {- First element in tuple is initial `Model`, and second is
           command to run when application loads.
        -}
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    -- Decodes integer located at `event.detail.userSlidTo`.
    at [ "detail", "userSlidTo" ] int
        -- Convert the integer to a message using toMsg.
        |> Json.Decode.map toMsg
        |> on "slide"
