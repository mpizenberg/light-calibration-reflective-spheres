port module Main exposing (main)

-- import Model exposing (BBox, FileDraggingState, Image, Model, Msg, NavigationMsg, Parameters, ParametersForm, ParametersToggleInfo, PointerMode, RunStep, State, defaultParams, defaultParamsForm, defaultParamsInfo, encodeMaybe, encodeParams, headerHeight, initialModel, progressBarHeight)

import Browser
import Browser.Dom
import Canvas.Texture
import Color
import Crop exposing (Crop)
import CropForm
import Device exposing (Device)
import Dict
import Element
import FileValue as File exposing (File)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode exposing (Decoder, Value)
import Json.Encode exposing (Value)
import Keyboard
import Model exposing (..)
import NumberInput
import Pivot exposing (Pivot)
import Set
import Task
import View exposing (..)
import Viewer exposing (Viewer)



-- Ports ###########################


port resizes : (Device.Size -> msg) -> Sub msg


port decodeImages : List Value -> Cmd msg


port loadImagesFromUrls : List String -> Cmd msg


port imageDecoded : ({ id : String, img : Value } -> msg) -> Sub msg


port capture : Value -> Cmd msg


port run : Value -> Cmd msg


port stop : () -> Cmd msg


port saveRegisteredImages : Int -> Cmd msg


port log : ({ lvl : Int, content : String } -> msg) -> Sub msg


port updateRunStep : ({ step : String, progress : Maybe Int } -> msg) -> Sub msg


port receiveCroppedImages : (List { id : String, img : Value } -> msg) -> Sub msg


port receiveCenters : (List { x : Int, y : Int } -> msg) -> Sub msg


main : Program Device.Size Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| Initialize the model.
-}
init : Device.Size -> ( Model, Cmd Msg )
init size =
    ( initialModel size, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Home _ ->
            Sub.batch [ resizes WindowResizes, log Log, imageDecoded ImageDecoded ]

        Loading _ ->
            Sub.batch [ resizes WindowResizes, log Log, imageDecoded ImageDecoded ]

        ViewImgs _ ->
            Sub.batch [ resizes WindowResizes, log Log, receiveCroppedImages ReceiveCroppedImages, updateRunStep UpdateRunStep, receiveCenters ReceiveCenters, Keyboard.downs KeyDown ]

        Config _ ->
            Sub.batch [ resizes WindowResizes, log Log, receiveCroppedImages ReceiveCroppedImages, receiveCenters ReceiveCenters, updateRunStep UpdateRunStep ]

        Registration _ ->
            Sub.batch [ resizes WindowResizes, log Log, receiveCroppedImages ReceiveCroppedImages, receiveCenters ReceiveCenters, updateRunStep UpdateRunStep, Keyboard.downs KeyDown ]

        Logs _ ->
            Sub.batch [ resizes WindowResizes, log Log, receiveCroppedImages ReceiveCroppedImages, receiveCenters ReceiveCenters, updateRunStep UpdateRunStep ]



-- Update #############################################################


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
        ( NoMsg, _ ) ->
            ( model, Cmd.none )

        ( WindowResizes size, _ ) ->
            ( { model
                | device = Device.classify size
                , viewer = Viewer.resize ( size.width, size.height - toFloat (Model.headerHeight + Model.progressBarHeight) ) model.viewer
                , registeredViewer = Viewer.resize ( size.width, size.height - toFloat (Model.headerHeight + Model.progressBarHeight) ) model.registeredViewer
              }
            , Cmd.none
            )

        ( DragDropMsg (DragOver _ _), Home _ ) ->
            ( { model | state = Home DraggingSomeFiles }, Cmd.none )

        ( DragDropMsg (Drop file otherFiles), Home _ ) ->
            let
                imageFiles =
                    List.filter (\f -> String.startsWith "image" f.mime) (file :: otherFiles)

                names =
                    Set.fromList (List.map .name imageFiles)
            in
            ( { model | state = Loading { names = names, loaded = Dict.empty } }
            , decodeImages (List.map File.encode imageFiles)
            )

        ( DragDropMsg DragLeave, Home _ ) ->
            ( { model | state = Home Idle }, Cmd.none )

        ( LoadExampleImages urls, _ ) ->
            ( { model | state = Loading { names = Set.fromList urls, loaded = Dict.empty } }
            , loadImagesFromUrls urls
            )

        ( ImageDecoded ({ id } as imgValue), Loading { names, loaded } ) ->
            let
                newLoaded =
                    case imageFromValue imgValue of
                        Nothing ->
                            -- Should never happen
                            loaded

                        Just image ->
                            Dict.insert id image loaded

                updatedLoadingState =
                    { names = names
                    , loaded = newLoaded
                    }

                oldParamsForm =
                    model.paramsForm
            in
            if Set.size names == Dict.size newLoaded then
                case Dict.values newLoaded of
                    [] ->
                        -- This should be impossible, there must be at least 1 image
                        ( { model | state = Home Idle }, Cmd.none )

                    firstImage :: otherImages ->
                        ( { model
                            | state = ViewImgs { images = Pivot.fromCons firstImage otherImages }
                            , viewer = Viewer.fitImage 1.0 ( toFloat firstImage.width, toFloat firstImage.height ) model.viewer
                            , paramsForm = { oldParamsForm | crop = CropForm.withSize firstImage.width firstImage.height }
                            , imagesCount = Set.size names
                          }
                        , Cmd.none
                        )

            else
                ( { model | state = Loading updatedLoadingState }, Cmd.none )

        ( KeyDown rawKey, ViewImgs { images } ) ->
            case Keyboard.navigationKey rawKey of
                Just Keyboard.ArrowRight ->
                    ( { model | state = ViewImgs { images = goToNextImage images } }, Cmd.none )

                Just Keyboard.ArrowLeft ->
                    ( { model | state = ViewImgs { images = goToPreviousImage images } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( ParamsMsg paramsMsg, Config _ ) ->
            ( updateParams paramsMsg model, Cmd.none )

        ( ParamsInfoMsg paramsInfoMsg, Config _ ) ->
            ( { model | paramsInfo = updateParamsInfo paramsInfoMsg model.paramsInfo }, Cmd.none )

        ( NavigationMsg navMsg, ViewImgs data ) ->
            ( goTo navMsg model data, Cmd.none )

        ( NavigationMsg navMsg, Config data ) ->
            ( goTo navMsg model data, Cmd.none )

        ( NavigationMsg navMsg, Registration data ) ->
            ( goTo navMsg model data, Cmd.none )

        ( NavigationMsg navMsg, Logs data ) ->
            ( goTo navMsg model data, Cmd.none )

        ( ZoomMsg zoomMsg, ViewImgs _ ) ->
            ( { model | viewer = zoomViewer zoomMsg model.viewer }, Cmd.none )

        ( ZoomMsg zoomMsg, Registration _ ) ->
            ( { model | registeredViewer = zoomViewer zoomMsg model.registeredViewer }, Cmd.none )

        ( PointerMsg pointerMsg, ViewImgs { images } ) ->
            case ( pointerMsg, model.pointerMode ) of
                -- Moving the viewer
                ( PointerDownRaw event, WaitingMove ) ->
                    case Json.Decode.decodeValue Pointer.eventDecoder event of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok { pointer } ->
                            ( { model | pointerMode = PointerMovingFromClientCoords pointer.clientPos }, capture event )

                ( PointerMove ( newX, newY ), PointerMovingFromClientCoords ( x, y ) ) ->
                    ( { model
                        | viewer = Viewer.pan ( newX - x, newY - y ) model.viewer
                        , pointerMode = PointerMovingFromClientCoords ( newX, newY )
                      }
                    , Cmd.none
                    )

                ( PointerUp _, PointerMovingFromClientCoords _ ) ->
                    ( { model | pointerMode = WaitingMove }, Cmd.none )

                -- Drawing the cropped area
                ( PointerDownRaw event, WaitingDraw ) ->
                    case Json.Decode.decodeValue Pointer.eventDecoder event of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok { pointer } ->
                            let
                                ( x, y ) =
                                    Viewer.coordinatesAt pointer.offsetPos model.viewer
                            in
                            ( { model
                                | pointerMode = PointerDrawFromOffsetAndClient pointer.offsetPos pointer.clientPos
                                , bboxDrawn = Just { left = x, top = y, right = x, bottom = y }
                              }
                            , capture event
                            )

                ( PointerMove ( newX, newY ), PointerDrawFromOffsetAndClient ( oX, oY ) ( cX, cY ) ) ->
                    let
                        ( x1, y1 ) =
                            Viewer.coordinatesAt ( oX, oY ) model.viewer

                        ( x2, y2 ) =
                            Viewer.coordinatesAt ( oX + newX - cX, oY + newY - cY ) model.viewer

                        left =
                            min x1 x2

                        top =
                            min y1 y2

                        right =
                            max x1 x2

                        bottom =
                            max y1 y2
                    in
                    ( { model | bboxDrawn = Just { left = left, top = top, right = right, bottom = bottom } }
                    , Cmd.none
                    )

                ( PointerUp _, PointerDrawFromOffsetAndClient _ _ ) ->
                    case model.bboxDrawn of
                        Just { left, right, top, bottom } ->
                            let
                                img =
                                    Pivot.getC (Pivot.goToStart images)

                                oldParams =
                                    model.params

                                oldParamsForm =
                                    model.paramsForm
                            in
                            if
                                -- sufficient width
                                ((right - left) / model.viewer.scale > 10)
                                    -- sufficient height
                                    && ((bottom - top) / model.viewer.scale > 10)
                                    -- at least one corner inside the image
                                    && (right > 0)
                                    && (left < toFloat img.width)
                                    && (bottom > 0)
                                    && (top < toFloat img.height)
                            then
                                let
                                    newCropForm =
                                        snapBBox (BBox left top right bottom) oldParamsForm.crop

                                    newCrop =
                                        CropForm.decoded newCropForm
                                in
                                ( { model
                                    | pointerMode = WaitingDraw
                                    , bboxDrawn = Maybe.map toBBox newCrop
                                    , params = { oldParams | crop = newCrop }
                                    , paramsForm = { oldParamsForm | crop = newCropForm }
                                  }
                                , Cmd.none
                                )

                            else
                                ( { model
                                    | pointerMode = WaitingDraw
                                    , bboxDrawn = Nothing
                                    , params = { oldParams | crop = Nothing }
                                    , paramsForm = { oldParamsForm | crop = CropForm.toggle False oldParamsForm.crop }
                                  }
                                , Cmd.none
                                )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( ViewImgMsg CropCurrentFrame, ViewImgs { images } ) ->
            let
                img =
                    Pivot.getC (Pivot.goToStart images)

                ( left, top ) =
                    model.viewer.origin

                ( width, height ) =
                    model.viewer.size

                right =
                    left + model.viewer.scale * width

                bottom =
                    top + model.viewer.scale * height

                oldParams =
                    model.params

                oldParamsForm =
                    model.paramsForm
            in
            if
                -- at least one corner inside the image
                (right > 0)
                    && (left < toFloat img.width)
                    && (bottom > 0)
                    && (top < toFloat img.height)
            then
                let
                    newCropForm =
                        snapBBox (BBox left top right bottom) oldParamsForm.crop

                    newCrop =
                        CropForm.decoded newCropForm
                in
                ( { model
                    | bboxDrawn = Maybe.map toBBox newCrop
                    , params = { oldParams | crop = newCrop }
                    , paramsForm = { oldParamsForm | crop = newCropForm }
                  }
                , Cmd.none
                )

            else
                ( { model
                    | bboxDrawn = Nothing
                    , params = { oldParams | crop = Nothing }
                    , paramsForm = { oldParamsForm | crop = CropForm.toggle False oldParamsForm.crop }
                  }
                , Cmd.none
                )

        ( ViewImgMsg SelectMovingMode, ViewImgs _ ) ->
            ( { model | pointerMode = WaitingMove }, Cmd.none )

        ( ViewImgMsg SelectDrawingMode, ViewImgs _ ) ->
            ( { model | pointerMode = WaitingDraw }, Cmd.none )

        ( ClickPreviousImage, ViewImgs { images } ) ->
            ( { model | state = ViewImgs { images = goToPreviousImage images } }, Cmd.none )

        ( ClickPreviousImage, Registration _ ) ->
            ( { model
                | registeredImages = Maybe.map goToPreviousImage model.registeredImages
                , registeredCenters = Maybe.map goToPreviousCenter model.registeredCenters
              }
            , Cmd.none
            )

        ( ClickNextImage, ViewImgs { images } ) ->
            ( { model | state = ViewImgs { images = goToNextImage images } }, Cmd.none )

        ( ClickNextImage, Registration _ ) ->
            ( { model
                | registeredImages = Maybe.map goToNextImage model.registeredImages
                , registeredCenters = Maybe.map goToNextCenter model.registeredCenters
              }
            , Cmd.none
            )

        ( RunAlgorithm params, ViewImgs imgs ) ->
            ( { model | state = Logs imgs, registeredImages = Nothing, runStep = StepNotStarted }, run (encodeParams params) )

        ( RunAlgorithm params, Config imgs ) ->
            ( { model | state = Logs imgs, registeredImages = Nothing, runStep = StepNotStarted }, run (encodeParams params) )

        ( RunAlgorithm params, Registration imgs ) ->
            ( { model | state = Logs imgs, registeredImages = Nothing, runStep = StepNotStarted }, run (encodeParams params) )

        ( RunAlgorithm params, Logs imgs ) ->
            ( { model | state = Logs imgs, registeredImages = Nothing, runStep = StepNotStarted }, run (encodeParams params) )

        ( StopRunning, _ ) ->
            ( { model | runStep = StepNotStarted }, stop () )

        ( UpdateRunStep { step, progress }, _ ) ->
            let
                runStep =
                    case ( model.runStep, step, progress ) of
                        _ ->
                            StepNotStarted
            in
            ( { model | runStep = runStep }, Cmd.none )

        ( Log logData, _ ) ->
            let
                newLogs =
                    logData :: model.logs
            in
            if model.autoscroll then
                ( { model | logs = newLogs }, scrollLogsToEndCmd )

            else
                ( { model | logs = newLogs }, Cmd.none )

        ( VerbosityChange floatVerbosity, _ ) ->
            ( { model | verbosity = round floatVerbosity }, Cmd.none )

        ( ScrollLogsToEnd, Logs _ ) ->
            ( model, scrollLogsToEndCmd )

        ( ToggleAutoScroll activate, _ ) ->
            if activate then
                ( { model | autoscroll = True }, scrollLogsToEndCmd )

            else
                ( { model | autoscroll = False }, Cmd.none )

        ( ReceiveCroppedImages croppedImages, _ ) ->
            case List.filterMap imageFromValue croppedImages of
                [] ->
                    ( model, Cmd.none )

                firstImage :: otherImages ->
                    ( { model
                        | registeredImages = Just (Pivot.fromCons firstImage otherImages)
                        , registeredViewer = Viewer.fitImage 1.0 ( toFloat firstImage.width, toFloat firstImage.height ) model.registeredViewer
                      }
                    , Cmd.none
                    )

        ( ReceiveCenters centers, _ ) ->
            case centers of
                [] ->
                    ( model, Cmd.none )

                firstCenter :: otherCenters ->
                    ( { model
                        | registeredCenters = Just (Pivot.fromCons firstCenter otherCenters)
                      }
                    , Cmd.none
                    )

        ( SaveRegisteredImages, _ ) ->
            ( model, saveRegisteredImages model.imagesCount )

        _ ->
            ( model, Cmd.none )



-- Misc #######################################


scrollLogsToEndCmd : Cmd Msg
scrollLogsToEndCmd =
    Task.attempt (\_ -> NoMsg) (Browser.Dom.setViewportOf "logs" 0 1.0e14)


imageFromValue : { id : String, img : Value } -> Maybe Image
imageFromValue { id, img } =
    case Canvas.Texture.fromDomImage img of
        Nothing ->
            Nothing

        Just texture ->
            let
                imgSize =
                    Canvas.Texture.dimensions texture
            in
            Just
                { id = id
                , texture = texture
                , width = round imgSize.width
                , height = round imgSize.height
                }


goToPreviousImage : Pivot Image -> Pivot Image
goToPreviousImage images =
    Maybe.withDefault (Pivot.goToEnd images) (Pivot.goL images)


goToPreviousCenter : Pivot { x : Int, y : Int } -> Pivot { x : Int, y : Int }
goToPreviousCenter centers =
    Maybe.withDefault (Pivot.goToEnd centers) (Pivot.goL centers)


goToNextImage : Pivot Image -> Pivot Image
goToNextImage images =
    Maybe.withDefault (Pivot.goToStart images) (Pivot.goR images)


goToNextCenter : Pivot { x : Int, y : Int } -> Pivot { x : Int, y : Int }
goToNextCenter centers =
    Maybe.withDefault (Pivot.goToStart centers) (Pivot.goR centers)


toBBox : Crop -> BBox
toBBox { left, top, right, bottom } =
    { left = toFloat left
    , top = toFloat top
    , right = toFloat right
    , bottom = toFloat bottom
    }


{-| Restrict coordinates of a drawn bounding box to the image dimension.
-}
snapBBox : BBox -> CropForm.State -> CropForm.State
snapBBox { left, top, right, bottom } state =
    let
        maxRight =
            -- Should never be Nothing
            Maybe.withDefault 0 state.right.max

        maxBottom =
            -- Should never be Nothing
            Maybe.withDefault 0 state.bottom.max

        leftCrop =
            round (max 0 left)

        topCrop =
            round (max 0 top)

        rightCrop =
            min (round right) maxRight

        bottomCrop =
            min (round bottom) maxBottom
    in
    CropForm.toggle True state
        |> CropForm.updateLeft (String.fromInt leftCrop)
        |> CropForm.updateTop (String.fromInt topCrop)
        |> CropForm.updateRight (String.fromInt rightCrop)
        |> CropForm.updateBottom (String.fromInt bottomCrop)


zoomViewer : ZoomMsg -> Viewer -> Viewer
zoomViewer msg viewer =
    case msg of
        ZoomFit img ->
            Viewer.fitImage 1.1 ( toFloat img.width, toFloat img.height ) viewer

        ZoomIn ->
            Viewer.zoomIn viewer

        ZoomOut ->
            Viewer.zoomOut viewer

        ZoomToward coordinates ->
            Viewer.zoomToward coordinates viewer

        ZoomAwayFrom coordinates ->
            Viewer.zoomAwayFrom coordinates viewer


goTo : NavigationMsg -> Model -> { images : Pivot Image } -> Model
goTo msg model data =
    case msg of
        GoToPageImages ->
            { model | state = ViewImgs data, pointerMode = WaitingMove }

        GoToPageConfig ->
            { model | state = Config data }

        GoToPageRegistration ->
            { model | state = Registration data }

        GoToPageLogs ->
            { model | state = Logs data }


updateParams : ParamsMsg -> Model -> Model
updateParams msg ({ params, paramsForm } as model) =
    case msg of
        ChangeMaxVerbosity str ->
            let
                updatedField =
                    NumberInput.updateInt str paramsForm.maxVerbosity

                updatedForm =
                    { paramsForm | maxVerbosity = updatedField }
            in
            case updatedField.decodedInput of
                Ok maxVerbosity ->
                    { model
                        | params = { params | maxVerbosity = maxVerbosity }
                        , paramsForm = updatedForm
                    }

                Err _ ->
                    { model | paramsForm = updatedForm }

        ToggleCrop activeCrop ->
            let
                newCropForm =
                    CropForm.toggle activeCrop paramsForm.crop
            in
            case ( activeCrop, CropForm.decoded newCropForm ) of
                ( True, Just crop ) ->
                    { model
                        | params = { params | crop = Just crop }
                        , paramsForm = { paramsForm | crop = newCropForm }
                        , bboxDrawn = Just (toBBox crop)
                    }

                _ ->
                    { model
                        | params = { params | crop = Nothing }
                        , paramsForm = { paramsForm | crop = newCropForm }
                        , bboxDrawn = Nothing
                    }

        ChangeCropLeft str ->
            changeCropSide (CropForm.updateLeft str) model

        ChangeCropTop str ->
            changeCropSide (CropForm.updateTop str) model

        ChangeCropRight str ->
            changeCropSide (CropForm.updateRight str) model

        ChangeCropBottom str ->
            changeCropSide (CropForm.updateBottom str) model

        ChangeSigma str ->
            let
                updatedField =
                    NumberInput.updateFloat str paramsForm.sigma

                updatedForm =
                    { paramsForm | sigma = updatedField }
            in
            case updatedField.decodedInput of
                Ok sigma ->
                    { model
                        | params = { params | sigma = sigma }
                        , paramsForm = updatedForm
                    }

                Err _ ->
                    { model | paramsForm = updatedForm }

        ChangeMaskRay str ->
            let
                updatedField =
                    NumberInput.updateFloat str paramsForm.maskRay

                updatedForm =
                    { paramsForm | maskRay = updatedField }
            in
            case updatedField.decodedInput of
                Ok maskRay ->
                    { model
                        | params = { params | maskRay = maskRay }
                        , paramsForm = updatedForm
                    }

                Err _ ->
                    { model | paramsForm = updatedForm }

        ChangeThreashold str ->
            let
                updatedField =
                    NumberInput.updateFloat str paramsForm.threashold

                updatedForm =
                    { paramsForm | threashold = updatedField }
            in
            case updatedField.decodedInput of
                Ok threashold ->
                    { model
                        | params = { params | threashold = threashold }
                        , paramsForm = updatedForm
                    }

                Err _ ->
                    { model | paramsForm = updatedForm }


changeCropSide : (CropForm.State -> CropForm.State) -> Model -> Model
changeCropSide updateSide model =
    let
        params =
            model.params

        paramsForm =
            model.paramsForm

        newCropForm =
            updateSide paramsForm.crop

        newCrop =
            CropForm.decoded newCropForm
    in
    { model
        | params = { params | crop = newCrop }
        , paramsForm = { paramsForm | crop = newCropForm }
        , bboxDrawn = Maybe.map toBBox newCrop
    }


updateParamsInfo : ParamsInfoMsg -> ParametersToggleInfo -> ParametersToggleInfo
updateParamsInfo msg toggleInfo =
    case msg of
        ToggleInfoCrop visible ->
            { toggleInfo | crop = visible }

        ToggleInfoMaxVerbosity visible ->
            { toggleInfo | maxVerbosity = visible }

        ToggleInfoSigma visible ->
            { toggleInfo | sigma = visible }

        ToggleInfoMaskRay visible ->
            { toggleInfo | maskRay = visible }

        ToggleInfoThreashold visible ->
            { toggleInfo | threashold = visible }
