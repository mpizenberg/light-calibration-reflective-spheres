port module Main exposing (main)

import Browser
import Browser.Dom
import Canvas.Texture
import Crop exposing (Crop)
import CropForm
import Device
import Dict
import File.Download as Dl
import FileValue as File
import Html.Events.Extra.Pointer as Pointer
import Json.Decode exposing (Value)
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


port receiveCroppedImages :
    ({ tl : List { id : String, img : Value }
     , tr : List { id : String, img : Value }
     , bl : List { id : String, img : Value }
     , br : List { id : String, img : Value }
     }
     -> msg
    )
    -> Sub msg


port receiveCenters :
    ({ tl : List { x : Int, y : Int }
     , tr : List { x : Int, y : Int }
     , bl : List { x : Int, y : Int }
     , br : List { x : Int, y : Int }
     }
     -> msg
    )
    -> Sub msg


port receiveLightDirs :
    (List { x : Float, y : Float, z : Float } -> msg)
    -> Sub msg


port receiveLightSources :
    (List { x : Float, y : Float, z : Float } -> msg)
    -> Sub msg


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
            Sub.batch [ resizes WindowResizes, log Log, receiveCroppedImages ReceiveCroppedImages, receiveCenters ReceiveCenters, receiveLightDirs ReceiveLightDirs, receiveLightSources ReceiveLightSources, updateRunStep UpdateRunStep, Keyboard.downs KeyDown ]

        Config _ ->
            Sub.batch [ resizes WindowResizes, log Log, receiveCroppedImages ReceiveCroppedImages, receiveCenters ReceiveCenters, receiveLightDirs ReceiveLightDirs, receiveLightSources ReceiveLightSources, updateRunStep UpdateRunStep ]

        Registration _ ->
            Sub.batch [ resizes WindowResizes, log Log, receiveCroppedImages ReceiveCroppedImages, receiveCenters ReceiveCenters, receiveLightDirs ReceiveLightDirs, receiveLightSources ReceiveLightSources, updateRunStep UpdateRunStep, Keyboard.downs KeyDown ]

        Logs _ ->
            Sub.batch [ resizes WindowResizes, log Log, receiveCroppedImages ReceiveCroppedImages, receiveCenters ReceiveCenters, receiveLightDirs ReceiveLightDirs, receiveLightSources ReceiveLightSources, updateRunStep UpdateRunStep ]

        Lighting _ ->
            Sub.batch [ resizes WindowResizes, log Log, receiveCroppedImages ReceiveCroppedImages, receiveCenters ReceiveCenters, receiveLightDirs ReceiveLightDirs, receiveLightSources ReceiveLightSources, updateRunStep UpdateRunStep, Keyboard.downs KeyDown ]



-- Update #############################################################


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
        ( NoMsg, _ ) ->
            ( model, Cmd.none )

        ( WindowResizes size, _ ) ->
            let
                oldRegisteredViewer =
                    model.registeredViewer
            in
            ( { model
                | device = Device.classify size
                , viewer = Viewer.resize ( size.width, size.height - toFloat (Model.headerHeight + Model.progressBarHeight) ) model.viewer
                , registeredViewer =
                    { tl = Viewer.resize ( size.width, size.height - toFloat (Model.headerHeight + Model.progressBarHeight) ) oldRegisteredViewer.tl
                    , tr = Viewer.resize ( size.width, size.height - toFloat (Model.headerHeight + Model.progressBarHeight) ) oldRegisteredViewer.tr
                    , bl = Viewer.resize ( size.width, size.height - toFloat (Model.headerHeight + Model.progressBarHeight) ) oldRegisteredViewer.bl
                    , br = Viewer.resize ( size.width, size.height - toFloat (Model.headerHeight + Model.progressBarHeight) ) oldRegisteredViewer.br
                    }
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

        ( KeyDown rawKey, Registration _ ) ->
            let
                oldRegisteredViewer =
                    model.registeredViewer
            in
            case Keyboard.navigationKey rawKey of
                Just Keyboard.ArrowRight ->
                    ( { model
                        | registeredViewer =
                            { tl = Viewer.pan ( -10.0, 0.0 ) oldRegisteredViewer.tl
                            , tr = Viewer.pan ( -10.0, 0.0 ) oldRegisteredViewer.tr
                            , bl = Viewer.pan ( -10.0, 0.0 ) oldRegisteredViewer.bl
                            , br = Viewer.pan ( -10.0, 0.0 ) oldRegisteredViewer.br
                            }
                      }
                    , Cmd.none
                    )

                Just Keyboard.ArrowLeft ->
                    ( { model
                        | registeredViewer =
                            { tl = Viewer.pan ( 10.0, 0.0 ) oldRegisteredViewer.tl
                            , tr = Viewer.pan ( 10.0, 0.0 ) oldRegisteredViewer.tr
                            , bl = Viewer.pan ( 10.0, 0.0 ) oldRegisteredViewer.bl
                            , br = Viewer.pan ( 10.0, 0.0 ) oldRegisteredViewer.br
                            }
                      }
                    , Cmd.none
                    )

                Just Keyboard.ArrowUp ->
                    ( { model
                        | registeredViewer =
                            { tl = Viewer.pan ( 0.0, 10.0 ) oldRegisteredViewer.tl
                            , tr = Viewer.pan ( 0.0, 10.0 ) oldRegisteredViewer.tr
                            , bl = Viewer.pan ( 0.0, 10.0 ) oldRegisteredViewer.bl
                            , br = Viewer.pan ( 0.0, 10.0 ) oldRegisteredViewer.br
                            }
                      }
                    , Cmd.none
                    )

                Just Keyboard.ArrowDown ->
                    ( { model
                        | registeredViewer =
                            { tl = Viewer.pan ( 0.0, -10.0 ) oldRegisteredViewer.tl
                            , tr = Viewer.pan ( 0.0, -10.0 ) oldRegisteredViewer.tr
                            , bl = Viewer.pan ( 0.0, -10.0 ) oldRegisteredViewer.bl
                            , br = Viewer.pan ( 0.0, -10.0 ) oldRegisteredViewer.br
                            }
                      }
                    , Cmd.none
                    )

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

        ( NavigationMsg navMsg, Lighting lightingData ) ->
            ( goTo navMsg model { images = lightingData.images }, Cmd.none )

        ( ZoomMsg zoomMsg, ViewImgs _ ) ->
            ( { model | viewer = zoomViewer zoomMsg model.viewer }, Cmd.none )

        ( ZoomMsg zoomMsg, Registration _ ) ->
            ( { model | registeredViewer = zoomViewerReg zoomMsg model.registeredViewer }, Cmd.none )

        ( PointerMsg pointerMsg, ViewImgs { images } ) ->
            let
                img =
                    Pivot.getC (Pivot.goToStart images)
            in
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

                                quadrant : Quadrant
                                quadrant =
                                    case ( x < toFloat img.width / 2, y < toFloat img.height / 2 ) of
                                        ( True, True ) ->
                                            TopLeft

                                        ( True, False ) ->
                                            BottomLeft

                                        ( False, True ) ->
                                            TopRight

                                        ( False, False ) ->
                                            BottomRight

                                oldBboxesDrawn : CroppedCircles
                                oldBboxesDrawn =
                                    model.bboxesDrawn

                                pointFrame : Maybe BBox
                                pointFrame =
                                    Just
                                        { left = x
                                        , top = y
                                        , right = x
                                        , bottom = y
                                        }
                            in
                            ( { model
                                | pointerMode = PointerDrawFromOffsetAndClient pointer.offsetPos pointer.clientPos
                                , bboxesDrawn =
                                    case quadrant of
                                        TopLeft ->
                                            { oldBboxesDrawn | topLeft = pointFrame }

                                        TopRight ->
                                            { oldBboxesDrawn | topRight = pointFrame }

                                        BottomLeft ->
                                            { oldBboxesDrawn | bottomLeft = pointFrame }

                                        BottomRight ->
                                            { oldBboxesDrawn | bottomRight = pointFrame }
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

                        centerXCrop : Float
                        centerXCrop =
                            right
                                + left
                                |> (*) 0.5

                        centerYCrop : Float
                        centerYCrop =
                            bottom
                                + top
                                |> (*) 0.5

                        centerXView : Float
                        centerXView =
                            img.width
                                |> toFloat
                                |> (*) 0.5

                        centerYView : Float
                        centerYView =
                            img.height
                                |> toFloat
                                |> (*) 0.5

                        quadrant : Quadrant
                        quadrant =
                            if centerXCrop > centerXView then
                                if centerYCrop > centerYView then
                                    BottomRight

                                else
                                    TopRight

                            else if centerYCrop > centerYView then
                                BottomLeft

                            else
                                TopLeft

                        oldBBoxes : CroppedCircles
                        oldBBoxes =
                            model.bboxesDrawn

                        newBoxes : CroppedCircles
                        newBoxes =
                            case quadrant of
                                TopLeft ->
                                    { oldBBoxes
                                        | topLeft =
                                            Just
                                                { left = left
                                                , right = right
                                                , top = top
                                                , bottom = bottom
                                                }
                                    }

                                TopRight ->
                                    { oldBBoxes
                                        | topRight =
                                            Just
                                                { left = left
                                                , right = right
                                                , top = top
                                                , bottom = bottom
                                                }
                                    }

                                BottomRight ->
                                    { oldBBoxes
                                        | bottomRight =
                                            Just
                                                { left = left
                                                , right = right
                                                , top = top
                                                , bottom = bottom
                                                }
                                    }

                                BottomLeft ->
                                    { oldBBoxes
                                        | bottomLeft =
                                            Just
                                                { left = left
                                                , right = right
                                                , top = top
                                                , bottom = bottom
                                                }
                                    }
                    in
                    ( { model
                        | bboxesDrawn =
                            newBoxes
                        , currentDrawingQuadrant = Just quadrant
                      }
                    , Cmd.none
                    )

                ( PointerUp _, PointerDrawFromOffsetAndClient _ _ ) ->
                    let
                        oldBBoxes : CroppedCircles
                        oldBBoxes =
                            model.bboxesDrawn

                        bboxDrawn : Maybe BBox
                        bboxDrawn =
                            case model.currentDrawingQuadrant of
                                Nothing ->
                                    Nothing

                                Just something ->
                                    case something of
                                        TopLeft ->
                                            model.bboxesDrawn.topLeft

                                        BottomLeft ->
                                            model.bboxesDrawn.bottomLeft

                                        TopRight ->
                                            model.bboxesDrawn.topRight

                                        BottomRight ->
                                            model.bboxesDrawn.bottomRight
                    in
                    case bboxDrawn of
                        -- type : BBox (floats)
                        Just { left, right, top, bottom } ->
                            let
                                oldParams =
                                    model.params

                                oldParamsForm =
                                    model.paramsForm

                                centerXCrop : Float
                                centerXCrop =
                                    right
                                        + left
                                        |> (*) 0.5

                                centerYCrop : Float
                                centerYCrop =
                                    bottom
                                        + top
                                        |> (*) 0.5

                                centerXView : Float
                                centerXView =
                                    img.width
                                        |> toFloat
                                        |> (*) 0.5

                                centerYView : Float
                                centerYView =
                                    img.height
                                        |> toFloat
                                        |> (*) 0.5

                                quadrant : Quadrant
                                quadrant =
                                    if centerXCrop > centerXView then
                                        if centerYCrop > centerYView then
                                            BottomRight

                                        else
                                            TopRight

                                    else if centerYCrop > centerYView then
                                        BottomLeft

                                    else
                                        TopLeft
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

                                    oldCrops : CropSet
                                    oldCrops =
                                        model.params.crops
                                in
                                case quadrant of
                                    TopLeft ->
                                        let
                                            newCrops : CropSet
                                            newCrops =
                                                { oldCrops
                                                    | topLeft =
                                                        Just
                                                            { left = round left
                                                            , right = round right
                                                            , top = round top
                                                            , bottom = round bottom
                                                            }
                                                }
                                        in
                                        ( { model
                                            | pointerMode = WaitingDraw
                                            , bboxesDrawn =
                                                { oldBBoxes
                                                    | topLeft =
                                                        Just
                                                            { left = left
                                                            , right = right
                                                            , top = top
                                                            , bottom = bottom
                                                            }
                                                }
                                            , params = { oldParams | crops = newCrops }
                                            , paramsForm = { oldParamsForm | crop = newCropForm }
                                          }
                                        , Cmd.none
                                        )

                                    BottomLeft ->
                                        let
                                            newCrops : CropSet
                                            newCrops =
                                                { oldCrops
                                                    | bottomLeft =
                                                        Just
                                                            { left = round left
                                                            , right = round right
                                                            , top = round top
                                                            , bottom = round bottom
                                                            }
                                                }
                                        in
                                        ( { model
                                            | pointerMode = WaitingDraw
                                            , bboxesDrawn =
                                                { oldBBoxes
                                                    | bottomLeft =
                                                        Just
                                                            { left = left
                                                            , right = right
                                                            , top = top
                                                            , bottom = bottom
                                                            }
                                                }
                                            , params = { oldParams | crops = newCrops }
                                            , paramsForm = { oldParamsForm | crop = newCropForm }
                                          }
                                        , Cmd.none
                                        )

                                    TopRight ->
                                        let
                                            newCrops : CropSet
                                            newCrops =
                                                { oldCrops
                                                    | topRight =
                                                        Just
                                                            { left = round left
                                                            , right = round right
                                                            , top = round top
                                                            , bottom = round bottom
                                                            }
                                                }
                                        in
                                        ( { model
                                            | pointerMode = WaitingDraw
                                            , bboxesDrawn =
                                                { oldBBoxes
                                                    | topRight =
                                                        Just
                                                            { left = left
                                                            , right = right
                                                            , top = top
                                                            , bottom = bottom
                                                            }
                                                }
                                            , params = { oldParams | crops = newCrops }
                                            , paramsForm = { oldParamsForm | crop = newCropForm }
                                          }
                                        , Cmd.none
                                        )

                                    BottomRight ->
                                        let
                                            newCrops : CropSet
                                            newCrops =
                                                { oldCrops
                                                    | bottomRight =
                                                        Just
                                                            { left = round left
                                                            , right = round right
                                                            , top = round top
                                                            , bottom = round bottom
                                                            }
                                                }
                                        in
                                        ( { model
                                            | pointerMode = WaitingDraw
                                            , bboxesDrawn =
                                                { oldBBoxes
                                                    | bottomRight =
                                                        Just
                                                            { left = left
                                                            , right = right
                                                            , top = top
                                                            , bottom = bottom
                                                            }
                                                }
                                            , params = { oldParams | crops = newCrops }
                                            , paramsForm = { oldParamsForm | crop = newCropForm }
                                            , currentDrawingQuadrant = Just quadrant
                                          }
                                        , Cmd.none
                                        )

                            else
                                ( { model
                                    | pointerMode = WaitingDraw
                                    , bboxesDrawn =
                                        { topLeft = Nothing
                                        , topRight = Nothing
                                        , bottomLeft = Nothing
                                        , bottomRight = Nothing
                                        }
                                    , params =
                                        { oldParams
                                            | crops =
                                                { topLeft = Nothing
                                                , topRight = Nothing
                                                , bottomLeft = Nothing
                                                , bottomRight = Nothing
                                                }
                                        }
                                    , paramsForm = { oldParamsForm | crop = CropForm.toggle False oldParamsForm.crop }
                                    , currentDrawingQuadrant = Nothing
                                  }
                                , Cmd.none
                                )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( PointerMsg pointerMsg, Registration _ ) ->
            case ( pointerMsg, model.pointerMode ) of
                -- Moving the viewer
                ( PointerDownRaw event, WaitingMove ) ->
                    case Json.Decode.decodeValue Pointer.eventDecoder event of
                        Err _ ->
                            ( { model | currentDrawingQuadrant = Just TopLeft }, Cmd.none )

                        Ok { pointer } ->
                            ( { model | pointerMode = PointerMovingFromClientCoords pointer.clientPos }, capture event )

                ( PointerMove ( newX, newY ), PointerMovingFromClientCoords ( x, y ) ) ->
                    let
                        oldRegisteredViewer =
                            model.registeredViewer

                        size : Device.Size
                        size =
                            model.device.size

                        quadrant : Quadrant
                        quadrant =
                            if x < (size.width / 2) then
                                if y < (size.height / 2) then
                                    TopLeft

                                else
                                    BottomLeft

                            else if y < (size.height / 2) then
                                TopRight

                            else
                                BottomRight

                        newRegisteredViewer =
                            case quadrant of
                                TopLeft ->
                                    { oldRegisteredViewer | tl = Viewer.pan ( newX - x, newY - y ) oldRegisteredViewer.tl }

                                TopRight ->
                                    { oldRegisteredViewer | tr = Viewer.pan ( newX - x, newY - y ) oldRegisteredViewer.tr }

                                BottomLeft ->
                                    { oldRegisteredViewer | bl = Viewer.pan ( newX - x, newY - y ) oldRegisteredViewer.bl }

                                BottomRight ->
                                    { oldRegisteredViewer | br = Viewer.pan ( newX - x, newY - y ) oldRegisteredViewer.br }
                    in
                    ( { model
                        | registeredViewer = newRegisteredViewer
                        , pointerMode = PointerMovingFromClientCoords ( newX, newY )
                      }
                    , Cmd.none
                    )

                ( PointerUp _, PointerMovingFromClientCoords _ ) ->
                    ( { model | pointerMode = WaitingMove }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( ViewImgMsg SelectMovingMode, ViewImgs _ ) ->
            ( { model | pointerMode = WaitingMove }, Cmd.none )

        ( ViewImgMsg SelectDrawingMode, ViewImgs _ ) ->
            ( { model | pointerMode = WaitingDraw }, Cmd.none )

        ( ClickPreviousImage, ViewImgs { images } ) ->
            ( { model | state = ViewImgs { images = goToPreviousImage images } }, Cmd.none )

        ( ClickPreviousImage, Registration _ ) ->
            ( { model
                | registeredImages = goToPreviousImageReg model.registeredImages
                , registeredCenters = goToPreviousCenterReg model.registeredCenters
              }
            , Cmd.none
            )

        ( ClickPreviousImage, Lighting lightingData ) ->
            ( { model
                | registeredLightDirs = Maybe.map goToPreviousLightDir model.registeredLightDirs
                , registeredLightSources = Maybe.map goToPreviousLightSource model.registeredLightSources
                , state = Lighting { images = goToPreviousImage lightingData.images, sources = Maybe.map goToPreviousLightSource lightingData.sources, dirs = Maybe.map goToPreviousLightDir lightingData.dirs }
              }
            , Cmd.none
            )

        ( ClickNextImage, ViewImgs { images } ) ->
            ( { model | state = ViewImgs { images = goToNextImage images } }, Cmd.none )

        ( ClickNextImage, Registration _ ) ->
            ( { model
                | registeredImages = goToNextImageReg model.registeredImages
                , registeredCenters = goToNextCenterReg model.registeredCenters
              }
            , Cmd.none
            )

        ( ClickNextImage, Lighting lightingData ) ->
            ( { model
                | registeredLightDirs = Maybe.map goToNextLightDir model.registeredLightDirs
                , registeredLightSources = Maybe.map goToNextLightSource model.registeredLightSources
                , state = Lighting { images = goToNextImage lightingData.images, sources = Maybe.map goToNextLightSource lightingData.sources, dirs = Maybe.map goToNextLightDir lightingData.dirs }
              }
            , Cmd.none
            )

        ( RunAlgorithm params, ViewImgs imgs ) ->
            ( { model | state = Logs imgs, registeredImages = { tl = Nothing, tr = Nothing, bl = Nothing, br = Nothing }, runStep = StepNotStarted }, run (encodeParams params) )

        ( RunAlgorithm params, Config imgs ) ->
            ( { model | state = Logs imgs, registeredImages = { tl = Nothing, tr = Nothing, bl = Nothing, br = Nothing }, runStep = StepNotStarted }, run (encodeParams params) )

        ( RunAlgorithm params, Registration imgs ) ->
            ( { model | state = Logs imgs, registeredImages = { tl = Nothing, tr = Nothing, bl = Nothing, br = Nothing }, runStep = StepNotStarted }, run (encodeParams params) )

        ( RunAlgorithm params, Logs imgs ) ->
            ( { model | state = Logs imgs, registeredImages = { tl = Nothing, tr = Nothing, bl = Nothing, br = Nothing }, runStep = StepNotStarted }, run (encodeParams params) )

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
            let
                croppedTL : Maybe (Pivot Image)
                croppedTL =
                    case List.filterMap imageFromValue croppedImages.tl of
                        [] ->
                            Nothing

                        firstImage :: otherImages ->
                            Just (Pivot.fromCons firstImage otherImages)

                croppedTR : Maybe (Pivot Image)
                croppedTR =
                    case List.filterMap imageFromValue croppedImages.tr of
                        [] ->
                            Nothing

                        firstImage :: otherImages ->
                            Just (Pivot.fromCons firstImage otherImages)

                croppedBL : Maybe (Pivot Image)
                croppedBL =
                    case List.filterMap imageFromValue croppedImages.bl of
                        [] ->
                            Nothing

                        firstImage :: otherImages ->
                            Just (Pivot.fromCons firstImage otherImages)

                croppedBR : Maybe (Pivot Image)
                croppedBR =
                    case List.filterMap imageFromValue croppedImages.br of
                        [] ->
                            Nothing

                        firstImage :: otherImages ->
                            Just (Pivot.fromCons firstImage otherImages)

                getSize : (Image -> Int) -> Pivot Image -> Float
                getSize dim piv =
                    piv
                        |> Pivot.getC
                        |> dim
                        |> toFloat

                oldRegisteredViewer =
                    model.registeredViewer
            in
            ( { model
                | registeredImages =
                    { tl = croppedTL
                    , tr = croppedTR
                    , bl = croppedBL
                    , br = croppedBR
                    }
                , registeredViewer =
                    { tl =
                        case croppedTL of
                            Nothing ->
                                oldRegisteredViewer.tl

                            Just piv ->
                                Viewer.fitImage 2.0 ( getSize .width piv, getSize .height piv ) oldRegisteredViewer.tl
                    , tr =
                        case croppedTR of
                            Nothing ->
                                oldRegisteredViewer.tr

                            Just piv ->
                                Viewer.fitImage 2.0 ( getSize .width piv, getSize .height piv ) oldRegisteredViewer.tr
                    , bl =
                        case croppedBL of
                            Nothing ->
                                oldRegisteredViewer.bl

                            Just piv ->
                                Viewer.fitImage 2.0 ( getSize .width piv, getSize .height piv ) oldRegisteredViewer.bl
                    , br =
                        case croppedBR of
                            Nothing ->
                                oldRegisteredViewer.br

                            Just piv ->
                                Viewer.fitImage 2.0 ( getSize .width piv, getSize .height piv ) oldRegisteredViewer.br
                    }
              }
            , Cmd.none
            )

        ( ReceiveCenters centers, _ ) ->
            let
                centersTL : Maybe (Pivot { x : Int, y : Int })
                centersTL =
                    case centers.tl of
                        [] ->
                            Nothing

                        firstCenter :: otherCenters ->
                            Just (Pivot.fromCons firstCenter otherCenters)

                centersTR : Maybe (Pivot { x : Int, y : Int })
                centersTR =
                    case centers.tr of
                        [] ->
                            Nothing

                        firstCenter :: otherCenters ->
                            Just (Pivot.fromCons firstCenter otherCenters)

                centersBL : Maybe (Pivot { x : Int, y : Int })
                centersBL =
                    case centers.bl of
                        [] ->
                            Nothing

                        firstCenter :: otherCenters ->
                            Just (Pivot.fromCons firstCenter otherCenters)

                centersBR : Maybe (Pivot { x : Int, y : Int })
                centersBR =
                    case centers.br of
                        [] ->
                            Nothing

                        firstCenter :: otherCenters ->
                            Just (Pivot.fromCons firstCenter otherCenters)
            in
            ( { model
                | registeredCenters =
                    { tl = centersTL
                    , tr = centersTR
                    , bl = centersBL
                    , br = centersBR
                    }
              }
            , Cmd.none
            )

        ( ReceiveLightDirs directions, _ ) ->
            let
                registrations : Maybe (Pivot Model.Point3D)
                registrations =
                    case directions of
                        [] ->
                            Nothing

                        firstDir :: otherDirs ->
                            Just (Pivot.fromCons firstDir otherDirs)
            in
            ( { model
                | registeredLightDirs = registrations
              }
            , Cmd.none
            )

        ( ReceiveLightSources sources, _ ) ->
            let
                registrations : Maybe (Pivot Model.Point3D)
                registrations =
                    case sources of
                        [] ->
                            Nothing

                        firstSource :: otherSources ->
                            Just (Pivot.fromCons firstSource otherSources)
            in
            ( { model
                | registeredLightSources = registrations
              }
            , Cmd.none
            )

        ( SaveRegisteredImages, _ ) ->
            ( model, saveRegisteredImages model.imagesCount )

        ( WriteLights, Lighting _ ) ->
            ( { model | downloadedLights = True }
            , case model.registeredLightDirs of
                Nothing -> Cmd.none
                Just dirs -> downloadLights dirs
            )

        _ ->
            ( model, Cmd.none )



-- Misc #######################################



downloadLights : (Pivot Point3D) -> Cmd msg
downloadLights dirs =
    dirs
        |> Pivot.toList
        |> List.map pointToString
        |> String.join " | "
        |> Dl.string "lights_orientations.lgts" "text/vectors"

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


goToPreviousImageReg :
    { tl : Maybe (Pivot Image)
    , tr : Maybe (Pivot Image)
    , bl : Maybe (Pivot Image)
    , br : Maybe (Pivot Image)
    }
    ->
        -- Pivot Image
        { tl : Maybe (Pivot Image)
        , tr : Maybe (Pivot Image)
        , bl : Maybe (Pivot Image)
        , br : Maybe (Pivot Image)
        }
goToPreviousImageReg images =
    { tl = Maybe.map goToPreviousImage images.tl
    , tr = Maybe.map goToPreviousImage images.tr
    , bl = Maybe.map goToPreviousImage images.bl
    , br = Maybe.map goToPreviousImage images.br
    }


goToPreviousCenter : Pivot { x : Int, y : Int } -> Pivot { x : Int, y : Int }
goToPreviousCenter centers =
    Maybe.withDefault (Pivot.goToEnd centers) (Pivot.goL centers)


goToPreviousCenterReg :
    { tl : Maybe (Pivot { x : Int, y : Int })
    , tr : Maybe (Pivot { x : Int, y : Int })
    , bl : Maybe (Pivot { x : Int, y : Int })
    , br : Maybe (Pivot { x : Int, y : Int })
    }
    ->
        { tl : Maybe (Pivot { x : Int, y : Int })
        , tr : Maybe (Pivot { x : Int, y : Int })
        , bl : Maybe (Pivot { x : Int, y : Int })
        , br : Maybe (Pivot { x : Int, y : Int })
        }
goToPreviousCenterReg centers =
    { tl = Maybe.map goToPreviousCenter centers.tl
    , tr = Maybe.map goToPreviousCenter centers.tr
    , bl = Maybe.map goToPreviousCenter centers.bl
    , br = Maybe.map goToPreviousCenter centers.br
    }


goToPreviousLightDir : Pivot Point3D -> Pivot Point3D
goToPreviousLightDir directions =
    Maybe.withDefault (Pivot.goToEnd directions) (Pivot.goL directions)


goToPreviousLightSource : Pivot Point3D -> Pivot Point3D
goToPreviousLightSource sources =
    Maybe.withDefault (Pivot.goToEnd sources) (Pivot.goL sources)


goToNextImage : Pivot Image -> Pivot Image
goToNextImage images =
    Maybe.withDefault (Pivot.goToStart images) (Pivot.goR images)


goToNextImageReg :
    { tl : Maybe (Pivot Image)
    , tr : Maybe (Pivot Image)
    , bl : Maybe (Pivot Image)
    , br : Maybe (Pivot Image)
    }
    ->
        -- Pivot Image
        { tl : Maybe (Pivot Image)
        , tr : Maybe (Pivot Image)
        , bl : Maybe (Pivot Image)
        , br : Maybe (Pivot Image)
        }
goToNextImageReg images =
    { tl = Maybe.map goToNextImage images.tl
    , tr = Maybe.map goToNextImage images.tr
    , bl = Maybe.map goToNextImage images.bl
    , br = Maybe.map goToNextImage images.br
    }


goToNextCenter : Pivot { x : Int, y : Int } -> Pivot { x : Int, y : Int }
goToNextCenter centers =
    Maybe.withDefault (Pivot.goToStart centers) (Pivot.goR centers)


goToNextCenterReg :
    { tl : Maybe (Pivot { x : Int, y : Int })
    , tr : Maybe (Pivot { x : Int, y : Int })
    , bl : Maybe (Pivot { x : Int, y : Int })
    , br : Maybe (Pivot { x : Int, y : Int })
    }
    ->
        { tl : Maybe (Pivot { x : Int, y : Int })
        , tr : Maybe (Pivot { x : Int, y : Int })
        , bl : Maybe (Pivot { x : Int, y : Int })
        , br : Maybe (Pivot { x : Int, y : Int })
        }
goToNextCenterReg centers =
    { tl = Maybe.map goToNextCenter centers.tl
    , tr = Maybe.map goToNextCenter centers.tr
    , bl = Maybe.map goToNextCenter centers.bl
    , br = Maybe.map goToNextCenter centers.br
    }


goToNextLightDir : Pivot Point3D -> Pivot Point3D
goToNextLightDir directions =
    Maybe.withDefault (Pivot.goToStart directions) (Pivot.goR directions)


goToNextLightSource : Pivot Point3D -> Pivot Point3D
goToNextLightSource sources =
    Maybe.withDefault (Pivot.goToStart sources) (Pivot.goR sources)


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

        -- Should not happen !!
        _ ->
            viewer


zoomViewerReg : ZoomMsg -> { tl : Viewer, tr : Viewer, bl : Viewer, br : Viewer } -> { tl : Viewer, tr : Viewer, bl : Viewer, br : Viewer }
zoomViewerReg msg viewer =
    case msg of
        ZoomInReg quadrant ->
            case quadrant of
                TopLeft ->
                    { viewer | tl = Viewer.zoomIn viewer.tl }

                TopRight ->
                    { viewer | tr = Viewer.zoomIn viewer.tr }

                BottomLeft ->
                    { viewer | bl = Viewer.zoomIn viewer.bl }

                BottomRight ->
                    { viewer | br = Viewer.zoomIn viewer.br }

        ZoomOutReg quadrant ->
            case quadrant of
                TopLeft ->
                    { viewer | tl = Viewer.zoomOut viewer.tl }

                TopRight ->
                    { viewer | tr = Viewer.zoomOut viewer.tr }

                BottomLeft ->
                    { viewer | bl = Viewer.zoomOut viewer.bl }

                BottomRight ->
                    { viewer | br = Viewer.zoomOut viewer.br }

        ZoomTowardReg quadrant coordinates ->
            case quadrant of
                TopLeft ->
                    { viewer | tl = Viewer.zoomToward coordinates viewer.tl }

                TopRight ->
                    { viewer | tr = Viewer.zoomToward coordinates viewer.tr }

                BottomLeft ->
                    { viewer | bl = Viewer.zoomToward coordinates viewer.bl }

                BottomRight ->
                    { viewer | br = Viewer.zoomToward coordinates viewer.br }

        ZoomAwayFromReg quadrant coordinates ->
            case quadrant of
                TopLeft ->
                    { viewer | tl = Viewer.zoomAwayFrom coordinates viewer.tl }

                TopRight ->
                    { viewer | tr = Viewer.zoomAwayFrom coordinates viewer.tr }

                BottomLeft ->
                    { viewer | bl = Viewer.zoomAwayFrom coordinates viewer.bl }

                BottomRight ->
                    { viewer | br = Viewer.zoomAwayFrom coordinates viewer.br }

        ZoomFitReg quadrant img ->
            case quadrant of
                TopLeft ->
                    { viewer | tl = Viewer.fitImage 1.1 ( toFloat img.width, toFloat img.height ) viewer.tl }

                TopRight ->
                    { viewer | tr = Viewer.fitImage 1.1 ( toFloat img.width, toFloat img.height ) viewer.tr }

                BottomLeft ->
                    { viewer | bl = Viewer.fitImage 1.1 ( toFloat img.width, toFloat img.height ) viewer.bl }

                BottomRight ->
                    { viewer | br = Viewer.fitImage 1.1 ( toFloat img.width, toFloat img.height ) viewer.br }

        -- Should not happen !
        _ ->
            viewer


goTo : NavigationMsg -> Model -> { images : Pivot Image } -> Model
goTo msg model data =
    case msg of
        GoToPageImages ->
            { model | state = ViewImgs data, pointerMode = WaitingMove }

        GoToPageConfig ->
            { model | state = Config data }

        GoToPageRegistration ->
            { model | state = Registration data, pointerMode = WaitingMove }

        GoToPageLogs ->
            { model | state = Logs data }

        GoToPageLighting ->
            { model | state = Lighting { sources = model.registeredLightSources, dirs = model.registeredLightDirs, images = data.images } }


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

                oldBboxes =
                    model.bboxesDrawn

                oldParams =
                    model.params

                oldCrops =
                    oldParams.crops
            in
            case ( activeCrop, CropForm.decoded newCropForm ) of
                ( True, Just crop ) ->
                    { model
                        | params = { oldParams | crops = { oldCrops | topLeft = Just crop } }
                        , paramsForm = { paramsForm | crop = newCropForm }
                        , bboxesDrawn = { oldBboxes | topLeft = Just (toBBox crop) }
                    }

                _ ->
                    { model
                        | params = { oldParams | crops = { oldCrops | topLeft = Nothing } }
                        , paramsForm = { paramsForm | crop = newCropForm }
                        , bboxesDrawn = { oldBboxes | topLeft = Nothing }
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

        oldCrops =
            params.crops

        paramsForm =
            model.paramsForm

        oldBboxes =
            model.bboxesDrawn

        newCropForm =
            updateSide paramsForm.crop

        newCrop : Maybe { left : Int, right : Int, top : Int, bottom : Int }
        newCrop =
            CropForm.decoded newCropForm
    in
    { model
        | params =
            { params
                | crops =
                    { oldCrops
                        | topLeft = newCrop
                    }
            }
        , paramsForm = { paramsForm | crop = newCropForm }
        , bboxesDrawn = { oldBboxes | topLeft = Maybe.map toBBox newCrop }
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
