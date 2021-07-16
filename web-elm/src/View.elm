module View exposing (..)

import Camera exposing (scene)
import Canvas
import Canvas.Settings
import Canvas.Settings.Advanced
import Canvas.Settings.Line
import Color
import CropForm
import Dict exposing (Dict)
import Element exposing (Element, alignBottom, alignLeft, alignRight, centerX, centerY, fill, fillPortion, height, padding, paddingXY, spacing, width)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import FileValue as File exposing (File)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Touch as Touch
import Html.Events.Extra.Wheel as Wheel
import Icon
import Json.Decode exposing (Decoder, Value)
import Model exposing (..)
import NumberInput
import Pivot exposing (Pivot)
import Set exposing (Set)
import Simple.Transition as Transition
import Style
import Viewer exposing (Viewer)
import Viewer.Canvas



-- View ##############################################################


view : Model -> Html Msg
view model =
    Element.layout [ Style.font, Element.clip ]
        (viewElmUI model)


viewElmUI : Model -> Element Msg
viewElmUI model =
    case model.state of
        Home draggingState ->
            viewHome draggingState

        Loading loadData ->
            viewLoading loadData

        ViewImgs { images } ->
            viewImgs model images

        Config { images } ->
            viewConfig model

        Registration { images } ->
            viewRegistration model

        Logs { images } ->
            viewLogs model

        Lighting lightingData ->
            viewLighting model lightingData



-- Header


headerBar : List ( PageHeader, Bool ) -> Element Msg
headerBar pages =
    Element.row
        [ height (Element.px headerHeight)
        , centerX
        ]
        (List.map (\( page, current ) -> pageHeaderElement current page) pages)


pageHeaderElement : Bool -> PageHeader -> Element Msg
pageHeaderElement current page =
    let
        bgColor =
            if current then
                Style.almostWhite

            else
                Style.white

        attributes =
            [ Element.Background.color bgColor
            , Element.htmlAttribute <| Html.Attributes.style "box-shadow" "none"
            , padding 10
            , height (Element.px headerHeight)
            ]

        i =
            1

        attributesLogs =
            [ Element.Background.color bgColor
            , padding 10
            , height (Element.px headerHeight)
            , Element.Border.dashed
            , Element.Border.widthXY 1 2
            , Element.Border.color
                (case i of
                    0 ->
                        Style.warningColor

                    1 ->
                        Style.errorColor

                    2 ->
                        Style.black

                    _ ->
                        Style.black
                )
            ]
    in
    case page of
        PageImages ->
            Element.Input.button attributes
                { onPress =
                    if current then
                        Nothing

                    else
                        Just (NavigationMsg GoToPageImages)
                , label = Element.text "Images"
                }

        PageConfig ->
            Element.Input.button attributes
                { onPress =
                    if current then
                        Nothing

                    else
                        Just (NavigationMsg GoToPageConfig)
                , label = Element.text "Config"
                }

        PageRegistration ->
            Element.Input.button attributes
                { onPress =
                    if current then
                        Nothing

                    else
                        Just (NavigationMsg GoToPageRegistration)
                , label = Element.text "Registration"
                }

        PageLogs ->
            Element.Input.button attributes
                { onPress =
                    if current then
                        Nothing

                    else
                        Just (NavigationMsg GoToPageLogs)
                , label = Element.text "Logs"
                }

        PageLighting ->
            Element.Input.button attributes
                { onPress =
                    if current then
                        Nothing

                    else
                        Just (NavigationMsg GoToPageLighting)
                , label = Element.text "Lighting"
                }



-- Run progress


{-| WARNING: this has to be kept consistent with the viewer size
-}
progressBarHeight : Int
progressBarHeight =
    38


runProgressBar : Model -> Element Msg
runProgressBar model =
    let
        progressBarRunButton =
            case model.runStep of
                StepNotStarted ->
                    runButton "Run ▶" model.params model.paramsForm

        progressBarStopButton =
            if model.runStep == StepNotStarted then
                Element.none

            else
                stopButton

        progressBarSaveButton =
            Element.none
    in
    Element.el
        [ width fill
        , height (Element.px progressBarHeight)
        , Element.Font.size 12
        , Element.behindContent (progressBar Style.almostWhite 1.0)
        , Element.behindContent (progressBar Style.runProgressColor <| 1.0)
        , Element.inFront progressBarRunButton
        , Element.inFront progressBarStopButton
        , Element.inFront progressBarSaveButton
        ]
        (Element.el [ centerX, centerY ] (Element.text <| progressMessage model))


progressMessage : Model -> String
progressMessage model =
    case model.runStep of
        StepNotStarted ->
            ""


estimateProgress : Model -> Float
estimateProgress model =
    let
        subprogress n nCount =
            toFloat n / toFloat nCount
    in
    case model.runStep of
        StepNotStarted ->
            0.0


runButton : String -> Parameters -> ParametersForm -> Element Msg
runButton content params paramsForm =
    let
        hasNoError =
            List.isEmpty (CropForm.errors paramsForm.crop)
    in
    if hasNoError then
        Element.Input.button
            [ centerX
            , padding 12
            , Element.Border.solid
            , Element.Border.width 1
            , Element.Border.rounded 4
            ]
            { onPress = Just (RunAlgorithm params), label = Element.text content }

    else
        Element.Input.button
            [ centerX
            , padding 12
            , Element.Border.solid
            , Element.Border.width 1
            , Element.Border.rounded 4
            , Element.Font.color Style.lightGrey
            ]
            { onPress = Nothing, label = Element.text content }


stopButton : Element Msg
stopButton =
    Element.Input.button
        [ alignRight
        , padding 12
        , Element.Border.solid
        , Element.Border.width 1
        , Element.Border.rounded 4
        ]
        { onPress = Just StopRunning
        , label = Element.text "Stop!"
        }


saveButton : Element Msg
saveButton =
    Element.Input.button
        [ alignRight
        , padding 12
        , Element.Border.solid
        , Element.Border.width 1
        , Element.Border.rounded 4
        ]
        { onPress = Just SaveRegisteredImages
        , label = Element.text "Save registered images"
        }


progressBar : Element.Color -> Float -> Element Msg
progressBar color progressRatio =
    let
        scaleX =
            "scaleX(" ++ String.fromFloat progressRatio ++ ")"
    in
    Element.el
        [ width fill
        , height fill
        , Element.Background.color color
        , Element.htmlAttribute (Html.Attributes.style "transform-origin" "top left")
        , Element.htmlAttribute (Html.Attributes.style "transform" scaleX)
        ]
        Element.none



-- Logs


viewLogs : Model -> Element Msg
viewLogs ({ autoscroll, verbosity, logs } as model) =
    Element.column [ width fill, height fill ]
        [ headerBar
            [ ( PageImages, False )
            , ( PageConfig, False )
            , ( PageRegistration, False )
            , ( PageLogs, True )
            , ( PageLighting, False )
            ]
        , runProgressBar model
        , Element.column [ width fill, height fill, paddingXY 0 18, spacing 18 ]
            [ Element.el [ centerX ] (verbositySlider verbosity)
            , Element.row [ centerX, spacing 18 ]
                [ Element.el [ centerY ] (Element.text "auto scroll:")
                , Element.el [ centerY ] (Element.text "off")
                , toggle ToggleAutoScroll autoscroll 24 "autoscroll"
                , Element.el [ centerY ] (Element.text "on")
                ]
            , Element.column
                [ padding 18
                , height fill
                , width fill
                , centerX
                , Style.fontMonospace
                , Element.Font.size 18
                , Element.scrollbars
                , Element.htmlAttribute (Html.Attributes.id "logs")
                ]
                (List.filter (\l -> l.lvl <= verbosity) logs
                    |> List.reverse
                    |> List.map viewLog
                )
            ]
        ]


viewLog : { lvl : Int, content : String } -> Element msg
viewLog { lvl, content } =
    case lvl of
        0 ->
            Element.el
                [ Element.Font.color Style.errorColor
                , paddingXY 0 12
                , Element.onLeft
                    (Element.el [ height fill, paddingXY 0 4 ]
                        (Element.el
                            [ height fill
                            , width (Element.px 4)
                            , Element.Background.color Style.errorColor
                            , Element.moveLeft 6
                            ]
                            Element.none
                        )
                    )
                ]
                (Element.text content)

        1 ->
            Element.el
                [ Element.Font.color Style.warningColor
                , paddingXY 0 12
                , Element.onLeft
                    (Element.el [ height fill, paddingXY 0 4 ]
                        (Element.el
                            [ height fill
                            , width (Element.px 4)
                            , Element.Background.color Style.warningColor
                            , Element.moveLeft 6
                            ]
                            Element.none
                        )
                    )
                ]
                (Element.text content)

        _ ->
            Element.text content


verbositySlider : Int -> Element Msg
verbositySlider verbosity =
    let
        thumbSize =
            32

        circle color size =
            [ Element.Border.color color
            , width (Element.px size)
            , height (Element.px size)
            , Element.Border.rounded (size // 2)
            , Element.Border.width 2
            ]
    in
    Element.Input.slider
        [ width (Element.px 200)
        , height (Element.px thumbSize)
        , spacing 18

        -- Here is where we're creating/styling the "track"
        , Element.behindContent <|
            Element.row [ width fill, centerY ]
                [ Element.el (circle Style.errorColor thumbSize) Element.none
                , Element.el [ width fill ] Element.none
                , Element.el (circle Style.warningColor thumbSize) Element.none
                , Element.el [ width fill ] Element.none
                , Element.el (circle Style.lightGrey thumbSize) Element.none
                , Element.el [ width fill ] Element.none
                , Element.el (circle Style.lightGrey thumbSize) Element.none
                , Element.el [ width fill ] Element.none
                , Element.el (circle Style.lightGrey thumbSize) Element.none
                ]
        ]
        { onChange = VerbosityChange
        , label =
            Element.Input.labelLeft [ centerY, Element.Font.size 18 ]
                (Element.el [] (Element.text "  Verbosity\n(min -> max)"))
        , min = 0
        , max = 4
        , step = Just 1
        , value = toFloat verbosity

        -- Here is where we're creating the "thumb"
        , thumb =
            let
                color =
                    if verbosity == 0 then
                        Style.errorColor

                    else if verbosity == 1 then
                        Style.warningColor

                    else
                        Style.lightGrey
            in
            Element.Input.thumb
                [ Element.Background.color color
                , width (Element.px thumbSize)
                , height (Element.px thumbSize)
                , Element.Border.rounded (thumbSize // 2)
                ]
        }



-- Parameters config


viewConfig : Model -> Element Msg
viewConfig ({ params, paramsForm, paramsInfo } as model) =
    Element.column [ width fill, height fill ]
        [ headerBar
            [ ( PageImages, False )
            , ( PageConfig, True )
            , ( PageRegistration, False )
            , ( PageLogs, False )
            , ( PageLighting, False )
            ]
        , runProgressBar model
        , Element.column [ width fill, height fill, Element.scrollbars ]
            [ Element.column [ paddingXY 20 32, spacing 32, centerX ]
                [ -- Title
                  Element.el [ Element.Font.center, Element.Font.size 32 ] (Element.text "Algorithm parameters")

                -- Cropped working frame
                , Element.column [ spacing 10 ]
                    [ Element.row [ spacing 10 ]
                        [ Element.text "Cropped working frame:"
                        , Element.Input.checkbox []
                            { onChange = ParamsInfoMsg << ToggleInfoCrop
                            , icon = infoIcon
                            , checked = paramsInfo.crop
                            , label = Element.Input.labelHidden "Show detail info about cropped working frame"
                            }
                        ]
                    , moreInfo paramsInfo.crop "Instead of using the whole image to estimate the registration, it is often faster and as accurate to focus the algorithm attention on a smaller frame in the image. The parameters here are the left, top, right and bottom coordinates of that cropped frame on which we want the algorithm to focus when estimating the alignment parameters."
                    , Element.row [ spacing 10 ]
                        [ Element.text "off"
                        , toggle (ParamsMsg << ToggleCrop) paramsForm.crop.active 30 "Toggle cropped working frame"
                        , Element.text "on"
                        ]
                    , CropForm.boxEditor
                        { changeLeft = ParamsMsg << ChangeCropLeft
                        , changeTop = ParamsMsg << ChangeCropTop
                        , changeRight = ParamsMsg << ChangeCropRight
                        , changeBottom = ParamsMsg << ChangeCropBottom
                        }
                        paramsForm.crop
                    , displayErrors (CropForm.errors paramsForm.crop)
                    ]

                -- Maximum verbosity
                , Element.column [ spacing 10 ]
                    [ Element.row [ spacing 10 ]
                        [ Element.text "Maximum verbosity:"
                        , Element.Input.checkbox []
                            { onChange = ParamsInfoMsg << ToggleInfoMaxVerbosity
                            , icon = infoIcon
                            , checked = paramsInfo.maxVerbosity
                            , label = Element.Input.labelHidden "Show detail info about the maximum verbosity."
                            }
                        ]
                    , moreInfo paramsInfo.maxVerbosity "Maximum verbosity of logs that can appear in the Logs tab. Setting this higher than its default value enables a very detailed log trace at the price of performance degradations."
                    , Element.text ("(default to " ++ String.fromInt defaultParams.maxVerbosity ++ ")")
                    , intInput paramsForm.maxVerbosity (ParamsMsg << ChangeMaxVerbosity) "Maximum verbosity"
                    , displayIntErrors paramsForm.maxVerbosity.decodedInput
                    ]

                -- Sigma variance
                , Element.column [ spacing 10 ]
                    [ Element.row [ spacing 10 ]
                        [ Element.text "Sigma variance of gaussian filter:"
                        , Element.Input.checkbox []
                            { onChange = ParamsInfoMsg << ToggleInfoSigma
                            , icon = infoIcon
                            , checked = paramsInfo.sigma
                            , label = Element.Input.labelHidden "Show detail info about the sigma parameter"
                            }
                        ]
                    , moreInfo paramsInfo.sigma "Sigma is the variance of the gaussian filter used to blur the ball. The grater the blurrier."
                    , Element.text ("(default to " ++ String.fromFloat defaultParams.sigma ++ ")")
                    , floatInput paramsForm.sigma (ParamsMsg << ChangeSigma) "Sigma"
                    , displayFloatErrors paramsForm.sigma.decodedInput
                    ]

                -- Mask ray
                , Element.column [ spacing 10 ]
                    [ Element.row [ spacing 10 ]
                        [ Element.text "Circular ray of the mask for lobe detection"
                        , Element.Input.checkbox []
                            { onChange = ParamsInfoMsg << ToggleInfoSigma
                            , icon = infoIcon
                            , checked = paramsInfo.maskRay
                            , label = Element.Input.labelHidden "Show detail info about the mask ray"
                            }
                        ]
                    , moreInfo paramsInfo.sigma "Before it appears to the screen, the ball is masked with a rectangular black layer, with a hole of a chosen ray. The smaller the ray, the greater the chance to avoid double specular lobes, but also the greater the chance to miss the OG lobe"
                    , Element.text ("(default to " ++ String.fromFloat defaultParams.maskRay ++ " < 1.0)")
                    , floatInput paramsForm.maskRay (ParamsMsg << ChangeMaskRay) "maskRay"
                    , displayFloatErrors paramsForm.maskRay.decodedInput
                    ]

                -- Gauss threashold
                , Element.column [ spacing 10 ]
                    [ Element.row [ spacing 10 ]
                        [ Element.text "Threashold for specular luminosity:"
                        , Element.Input.checkbox []
                            { onChange = ParamsInfoMsg << ToggleInfoThreashold
                            , icon = infoIcon
                            , checked = paramsInfo.threashold
                            , label = Element.Input.labelHidden "Show detail info about the threashold"
                            }
                        ]
                    , moreInfo paramsInfo.threashold "The threashold is the limit value of gray level above which we consider that a pixel belongs to the specular globe."
                    , Element.text ("(default to " ++ String.fromFloat defaultParams.threashold ++ " < 1.0)")
                    , floatInput paramsForm.threashold (ParamsMsg << ChangeThreashold) "Threashold"
                    , displayFloatErrors paramsForm.threashold.decodedInput
                    ]
                ]
            ]
        ]



-- Registration view


viewRegistration : Model -> Element Msg
viewRegistration ({ registeredImages, registeredViewer, registeredCenters } as model) =
    let
        littlechunk :
            ({ tl : Maybe (Pivot { x : Int, y : Int })
             , tr : Maybe (Pivot { x : Int, y : Int })
             , bl : Maybe (Pivot { x : Int, y : Int })
             , br : Maybe (Pivot { x : Int, y : Int })
             }
             -> Maybe (Pivot { x : Int, y : Int })
            )
            ->
                ({ tl : Viewer
                 , tr : Viewer
                 , bl : Viewer
                 , br : Viewer
                 }
                 -> Viewer
                )
            -> Quadrant
            -> Pivot Image
            -> Element Msg
        littlechunk chooseSide chooseViewer quadrant images =
            let
                ourViewer : Viewer
                ourViewer =
                    registeredViewer |> chooseViewer

                img =
                    Pivot.getC images

                clickButton alignment msg title icon =
                    Element.Input.button
                        [ padding 6
                        , alignment
                        , Element.Background.color (Element.rgba255 255 255 255 0.8)
                        , Element.Font.color Style.black
                        , Element.htmlAttribute <| Html.Attributes.style "box-shadow" "none"
                        , Element.htmlAttribute <| Html.Attributes.title title
                        ]
                        { onPress = Just msg
                        , label = icon 32
                        }

                buttonsRow =
                    Element.row [ centerX ]
                        [ clickButton centerX (ZoomMsg (ZoomFitReg quadrant img)) "Fit zoom to image" Icon.zoomFit
                        , clickButton centerX (ZoomMsg (ZoomOutReg quadrant)) "Zoom out" Icon.zoomOut
                        , clickButton centerX (ZoomMsg (ZoomInReg quadrant)) "Zoom in" Icon.zoomIn
                        ]

                ( viewerWidth, viewerHeight ) =
                    ourViewer
                        |> .size
                        |> Tuple.mapBoth ((*) 0.5) ((*) 0.5)

                clearCanvas : Canvas.Renderable
                clearCanvas =
                    Canvas.clear ( 0, 0 ) viewerWidth viewerHeight

                ray : Float
                ray =
                    toFloat (img.width + img.height) / 4

                renderedImage : Canvas.Renderable
                renderedImage =
                    Canvas.texture
                        [ ourViewer |> Viewer.Canvas.transform
                        , Canvas.Settings.Advanced.imageSmoothing False
                        ]
                        ( -ray, -ray )
                        img.texture

                canvasViewer : Maybe (Pivot { x : Int, y : Int }) -> Html Msg
                canvasViewer centersList =
                    Canvas.toHtml ( round viewerWidth, round viewerHeight )
                        [ Html.Attributes.id "theCanvas"
                        , Html.Attributes.style "display" "block"
                        , Wheel.onWheel (ourViewer |> zoomWheelRegMsg quadrant)
                        , msgOn "pointerdown" (Json.Decode.map (PointerMsg << PointerDownRaw) Json.Decode.value)
                        , Pointer.onUp (\e -> PointerMsg (PointerUp e.pointer.offsetPos))
                        , Html.Attributes.style "touch-action" "none"
                        , Html.Events.preventDefaultOn "pointermove" <|
                            Json.Decode.map (\coords -> ( PointerMsg (PointerMove coords), True )) <|
                                Json.Decode.map2 Tuple.pair
                                    (Json.Decode.field "clientX" Json.Decode.float)
                                    (Json.Decode.field "clientY" Json.Decode.float)
                        ]
                        [ clearCanvas
                        , renderedImage
                        , let
                            c : { x : Float, y : Float }
                            c =
                                case centersList of
                                    Nothing ->
                                        { x = 0.0, y = 0.0 }

                                    Just centersPivot ->
                                        Pivot.getC centersPivot
                                            |> (\p -> { x = toFloat p.x, y = toFloat p.y })
                          in
                          Canvas.shapes
                            [ Canvas.Settings.stroke (Color.rgba 1 0 0 0.7)
                            , Canvas.Settings.Line.lineWidth
                                (ourViewer
                                    |> .scale
                                    |> (*) 3
                                )
                            , ourViewer |> Viewer.Canvas.transform
                            ]
                            [ Canvas.path ( c.x - ray, 0.0 - ray )
                                [ Canvas.lineTo ( c.x - ray, viewerHeight - ray ) ]
                            , Canvas.path ( 0.0 - ray, c.y - ray )
                                [ Canvas.lineTo ( viewerWidth - ray, c.y - ray ) ]
                            ]
                        ]
            in
            Element.el
                [ Element.inFront buttonsRow
                , Element.inFront
                    (Element.row [ alignBottom, width fill ]
                        [ clickButton alignLeft ClickPreviousImage "Previous image" Icon.arrowLeftCircle
                        , clickButton alignRight ClickNextImage "Next image" Icon.arrowRightCircle
                        ]
                    )
                , Element.clip
                , Element.Border.dotted
                , Element.Border.width 3
                , height (fillPortion 2)
                , width (fillPortion 2)
                ]
                (registeredCenters
                    |> chooseSide
                    |> canvasViewer
                    |> Element.html
                )

        viewTL : Element Msg
        viewTL =
            case registeredImages.tl of
                Nothing ->
                    Element.el [ centerX, centerY, Element.Border.dotted, Element.Border.width 3, height (fillPortion 2), width (fillPortion 2) ]
                        (Element.text "Registration not done yet")

                Just images ->
                    littlechunk .tl .tl TopLeft images

        viewTR : Element Msg
        viewTR =
            case registeredImages.tr of
                Nothing ->
                    Element.el [ centerX, centerY, Element.Border.dotted, Element.Border.width 3, height (fillPortion 2), width (fillPortion 2) ]
                        (Element.text "Registration not done yet")

                Just images ->
                    littlechunk .tr .tr TopRight images

        viewBL : Element Msg
        viewBL =
            case registeredImages.bl of
                Nothing ->
                    Element.el [ centerX, centerY, Element.Border.dotted, Element.Border.width 3, height (fillPortion 2), width (fillPortion 2) ]
                        (Element.text "Registration not done yet")

                Just images ->
                    littlechunk .bl .bl BottomLeft images

        viewBR : Element Msg
        viewBR =
            case registeredImages.br of
                Nothing ->
                    Element.el [ centerX, centerY, Element.Border.dotted, Element.Border.width 3, height (fillPortion 2), width (fillPortion 2) ]
                        (Element.text "Registration not done yet")

                Just images ->
                    littlechunk .br .br BottomRight images

        bigchunk : Element Msg
        bigchunk =
            Element.row [ width fill, height fill ]
                [ Element.column [ width fill, height fill ]
                    [ viewTL
                    , viewBL
                    ]
                , Element.column [ width fill, height fill ]
                    [ viewTR
                    , viewBR
                    ]
                ]
    in
    Element.column [ width fill, height fill ]
        [ headerBar
            [ ( PageImages, False )
            , ( PageConfig, False )
            , ( PageRegistration, True )
            , ( PageLogs, False )
            , ( PageLighting, False )
            ]
        , runProgressBar model
        , Element.html <|
            Html.node "style"
                []
                [ Html.text ".pixelated { image-rendering: pixelated; image-rendering: crisp-edges; }" ]
        , bigchunk
        ]



-- view lighting


viewLighting : Model -> { sources : Maybe (Pivot Point3D), dirs : Maybe (Pivot Point3D), images : Pivot Image } -> Element Msg
viewLighting model ({ sources, dirs, images } as lightingData) =
    let
        source : Point3D
        source =
            case sources of
                Nothing ->
                    { x = 0.0, y = 0.0, z = 0.0 }

                Just pivot ->
                    Pivot.getC pivot

        dir : Point3D
        dir =
            case dirs of
                Nothing ->
                    { x = 0.0, y = 0.0, z = 0.0 }

                Just pivot ->
                    Pivot.getC pivot

        clickButton : Element.Attribute Msg -> Msg -> String -> (Float -> Element Msg) -> Element Msg
        clickButton alignment msg title icon =
            let
                strokeColor =
                    Style.lightGrey
            in
            Element.Input.button
                [ padding 6
                , alignment
                , Element.Background.color (Element.rgba255 255 255 255 0.8)
                , Element.Font.color strokeColor
                , Element.htmlAttribute <| Html.Attributes.style "box-shadow" "none"
                , Element.htmlAttribute <| Html.Attributes.title title
                ]
                { onPress = Just msg
                , label = icon 32
                }
    in
    Element.column
        [ width fill
        , height fill
        , Element.inFront
            (Element.row [ alignBottom, width fill ]
                [ clickButton alignLeft ClickPreviousImage "Previous image" Icon.arrowLeftCircle
                , clickButton alignRight ClickNextImage "Next image" Icon.arrowRightCircle
                ]
            )
        ]
        [ headerBar
            [ ( PageImages, False )
            , ( PageConfig, False )
            , ( PageRegistration, False )
            , ( PageLogs, False )
            , ( PageLighting, True )
            ]
        , Element.html <|
            scene
        , Element.text "Source :"
        , Element.text ("x: " ++ String.fromFloat source.x ++ " | y: " ++ String.fromFloat source.y ++ " | z: " ++ String.fromFloat source.z)
        , Element.text "Direction :"
        , Element.text ("x: " ++ String.fromFloat dir.x ++ " | y: " ++ String.fromFloat dir.y ++ " | z: " ++ String.fromFloat dir.z)
        , Element.text
            ("Sizes : "
                ++ (case dirs of
                        Nothing ->
                            "Empty"

                        Just p ->
                            p |> Pivot.lengthA |> String.fromInt
                   )
            )
        ]



-- More info


moreInfo : Bool -> String -> Element msg
moreInfo visible message =
    if not visible then
        Element.none

    else
        Element.paragraph
            [ Element.Background.color Style.almostWhite
            , padding 10
            , Element.Font.size 14
            , width (Element.maximum 400 fill)
            ]
            [ Element.text message ]


infoIcon : Bool -> Element msg
infoIcon detailsVisible =
    if detailsVisible then
        Element.el
            [ Element.Border.width 1
            , Element.Border.rounded 4
            , Element.Font.center
            , width (Element.px 24)
            , height (Element.px 24)
            , Element.Border.solid
            , Element.Background.color Style.almostWhite
            ]
            (Element.text "?")

    else
        Element.el
            [ Element.Border.width 1
            , Element.Border.rounded 4
            , Element.Font.center
            , width (Element.px 24)
            , height (Element.px 24)
            , Element.Border.dashed
            ]
            (Element.text "?")



-- Crop input


displayErrors : List String -> Element msg
displayErrors errors =
    if List.isEmpty errors then
        Element.none

    else
        Element.column [ spacing 10, Element.Font.size 14, Element.Font.color Style.errorColor ]
            (List.map (\err -> Element.paragraph [] [ Element.text err ]) errors)



-- Int input


displayIntErrors : Result (List NumberInput.IntError) a -> Element msg
displayIntErrors result =
    case result of
        Ok _ ->
            Element.none

        Err errors ->
            displayErrors (List.map (NumberInput.intErrorToString { valueName = "Value" }) errors)


intInput : NumberInput.Field Int NumberInput.IntError -> (String -> msg) -> String -> Element msg
intInput field msgTag label =
    let
        textField =
            Element.Input.text [ Element.Border.width 0, Element.Font.center, width (Element.px 100) ]
                { onChange = msgTag
                , text = field.input
                , placeholder = Nothing
                , label = Element.Input.labelHidden label
                }
    in
    case field.decodedInput of
        Err _ ->
            Element.row
                [ Element.Border.solid
                , Element.Border.width 1
                , Element.Border.rounded 4
                , Element.Font.color Style.errorColor
                ]
                [ numberSideButton Nothing "−"
                , textField
                , numberSideButton Nothing "+"
                ]

        Ok current ->
            let
                increased =
                    field.increase current

                decreased =
                    field.decrease current

                decrementMsg =
                    case field.min of
                        Nothing ->
                            Just (msgTag (String.fromInt decreased))

                        Just minBound ->
                            if current <= minBound then
                                Nothing

                            else
                                Just (msgTag (String.fromInt <| max decreased minBound))

                incrementMsg =
                    case field.max of
                        Nothing ->
                            Just (msgTag (String.fromInt increased))

                        Just maxBound ->
                            if current >= maxBound then
                                Nothing

                            else
                                Just (msgTag (String.fromInt <| min increased maxBound))
            in
            Element.row [ Element.Border.solid, Element.Border.width 1, Element.Border.rounded 4 ]
                [ numberSideButton decrementMsg "−"
                , textField
                , numberSideButton incrementMsg "+"
                ]


numberSideButton : Maybe msg -> String -> Element msg
numberSideButton maybeMsg label =
    let
        textColor =
            if maybeMsg == Nothing then
                Style.lightGrey

            else
                Style.black
    in
    Element.Input.button
        [ height fill
        , width (Element.px 44)
        , Element.Font.center
        , Element.Font.color textColor
        , Element.htmlAttribute <| Html.Attributes.style "box-shadow" "none"
        ]
        { onPress = maybeMsg, label = Element.text label }



-- Float input


displayFloatErrors : Result (List NumberInput.FloatError) a -> Element msg
displayFloatErrors result =
    case result of
        Ok _ ->
            Element.none

        Err errors ->
            displayErrors (List.map (NumberInput.floatErrorToString { valueName = "Value" }) errors)


floatInput : NumberInput.Field Float NumberInput.FloatError -> (String -> msg) -> String -> Element msg
floatInput field msgTag label =
    let
        textField =
            Element.Input.text [ Element.Border.width 0, Element.Font.center, width (Element.px 140) ]
                { onChange = msgTag
                , text = field.input
                , placeholder = Nothing
                , label = Element.Input.labelHidden label
                }
    in
    case field.decodedInput of
        Err _ ->
            Element.row
                [ Element.Border.solid
                , Element.Border.width 1
                , Element.Border.rounded 4
                , Element.Font.color Style.errorColor
                ]
                [ numberSideButton Nothing "−"
                , textField
                , numberSideButton Nothing "+"
                ]

        Ok current ->
            let
                increased =
                    field.increase current

                decreased =
                    field.decrease current

                decrementMsg =
                    case field.min of
                        Nothing ->
                            Just (msgTag (String.fromFloat decreased))

                        Just minBound ->
                            if current <= minBound then
                                Nothing

                            else
                                Just (msgTag (String.fromFloat <| max decreased minBound))

                incrementMsg =
                    case field.max of
                        Nothing ->
                            Just (msgTag (String.fromFloat increased))

                        Just maxBound ->
                            if current >= maxBound then
                                Nothing

                            else
                                Just (msgTag (String.fromFloat <| min increased maxBound))
            in
            Element.row [ Element.Border.solid, Element.Border.width 1, Element.Border.rounded 4 ]
                [ numberSideButton decrementMsg "−"
                , textField
                , numberSideButton incrementMsg "+"
                ]



-- toggle


toggle : (Bool -> Msg) -> Bool -> Float -> String -> Element Msg
toggle msg checked toggleHeight label =
    Element.Input.checkbox [] <|
        { onChange = msg
        , label = Element.Input.labelHidden label
        , checked = checked
        , icon =
            toggleCheckboxWidget
                { offColor = Style.lightGrey
                , onColor = Style.green
                , sliderColor = Style.white
                , toggleWidth = 2 * round toggleHeight
                , toggleHeight = round toggleHeight
                }
        }


toggleCheckboxWidget : { offColor : Element.Color, onColor : Element.Color, sliderColor : Element.Color, toggleWidth : Int, toggleHeight : Int } -> Bool -> Element msg
toggleCheckboxWidget { offColor, onColor, sliderColor, toggleWidth, toggleHeight } checked =
    let
        pad =
            3

        sliderSize =
            toggleHeight - 2 * pad

        translation =
            (toggleWidth - sliderSize - pad)
                |> String.fromInt
    in
    Element.el
        [ Element.Background.color <|
            if checked then
                onColor

            else
                offColor
        , Element.width <| Element.px <| toggleWidth
        , Element.height <| Element.px <| toggleHeight
        , Element.Border.rounded (toggleHeight // 2)
        , Element.inFront <|
            Element.el [ Element.height Element.fill ] <|
                Element.el
                    [ Element.Background.color sliderColor
                    , Element.Border.rounded <| sliderSize // 2
                    , Element.width <| Element.px <| sliderSize
                    , Element.height <| Element.px <| sliderSize
                    , Element.centerY
                    , Element.moveRight pad
                    , Element.htmlAttribute <|
                        Html.Attributes.style "transition" ".4s"
                    , Element.htmlAttribute <|
                        if checked then
                            Html.Attributes.style "transform" <| "translateX(" ++ translation ++ "px)"

                        else
                            Html.Attributes.class ""
                    ]
                    Element.none
        ]
        Element.none



-- View Images


viewImgs : Model -> Pivot Image -> Element Msg
viewImgs ({ pointerMode, bboxesDrawn, viewer } as model) images =
    let
        img =
            Pivot.getC images

        clickButton alignment abled msg title icon =
            let
                strokeColor =
                    if abled then
                        Style.black

                    else
                        Style.lightGrey
            in
            Element.Input.button
                [ padding 6
                , alignment
                , Element.Background.color (Element.rgba255 255 255 255 0.8)
                , Element.Font.color strokeColor
                , Element.htmlAttribute <| Html.Attributes.style "box-shadow" "none"
                , Element.htmlAttribute <| Html.Attributes.title title
                ]
                { onPress = Just msg
                , label = icon 32
                }

        modeButton selected msg title icon =
            let
                ( bgColor, action ) =
                    if selected then
                        ( Style.lightGrey, Nothing )

                    else
                        ( Element.rgba 255 255 255 0.8, Just msg )
            in
            Element.Input.button
                [ padding 6
                , centerX
                , Element.Background.color bgColor
                , Element.htmlAttribute <| Html.Attributes.style "box-shadow" "none"
                , Element.htmlAttribute <| Html.Attributes.title title
                ]
                { onPress = action
                , label = icon 32
                }

        isMovingMode =
            case pointerMode of
                WaitingMove ->
                    True

                PointerMovingFromClientCoords _ ->
                    True

                WaitingDraw ->
                    False

                PointerDrawFromOffsetAndClient _ _ ->
                    False

        buttonsRow =
            Element.row [ width fill ]
                [ clickButton centerX True (ZoomMsg (ZoomFit img)) "Fit zoom to image" Icon.zoomFit
                , clickButton centerX True (ZoomMsg ZoomOut) "Zoom out" Icon.zoomOut
                , clickButton centerX True (ZoomMsg ZoomIn) "Zoom in" Icon.zoomIn
                , modeButton isMovingMode (ViewImgMsg SelectMovingMode) "Move mode" Icon.move
                , Element.el [ width (Element.maximum 100 fill) ] Element.none
                , modeButton (not isMovingMode) (ViewImgMsg SelectDrawingMode) "Draw the cropped working area as a bounding box" Icon.boundingBox

                -- , clickButton centerX True (ViewImgMsg CropCurrentFrame) "Set the cropped working area to the current frame" Icon.maximize
                ]

        ( viewerWidth, viewerHeight ) =
            viewer.size

        clearCanvas : Canvas.Renderable
        clearCanvas =
            Canvas.clear ( 0, 0 ) viewerWidth viewerHeight

        renderedImage : Canvas.Renderable
        renderedImage =
            Canvas.texture
                [ Viewer.Canvas.transform viewer
                , Canvas.Settings.Advanced.imageSmoothing False
                ]
                ( 0, 0 )
                img.texture

        renderedBboxTopLeft : List Canvas.Shape
        renderedBboxTopLeft =
            case bboxesDrawn.topLeft of
                Nothing ->
                    []

                Just { left, top, right, bottom } ->
                    let
                        bboxWidth : Float
                        bboxWidth =
                            right - left

                        bboxHeight : Float
                        bboxHeight =
                            bottom - top

                        ray : Float
                        ray =
                            sqrt
                                ((bboxWidth * bboxWidth)
                                    + (bboxHeight * bboxHeight)
                                )
                                |> (*) 0.5

                        cxCircle : Float
                        cxCircle =
                            left + right |> (*) 0.5

                        cyCircle : Float
                        cyCircle =
                            top + bottom |> (*) 0.5
                    in
                    [ Canvas.circle ( cxCircle, cyCircle ) ray
                    , Canvas.path ( left, top )
                        [ Canvas.lineTo ( right, bottom ) ]
                    , Canvas.path ( right, top )
                        [ Canvas.lineTo ( left, bottom ) ]
                    ]

        renderedBboxTopRight : List Canvas.Shape
        renderedBboxTopRight =
            case bboxesDrawn.topRight of
                Nothing ->
                    []

                Just { left, top, right, bottom } ->
                    let
                        bboxWidth : Float
                        bboxWidth =
                            right - left

                        bboxHeight : Float
                        bboxHeight =
                            bottom - top

                        ray : Float
                        ray =
                            sqrt
                                ((bboxWidth * bboxWidth)
                                    + (bboxHeight * bboxHeight)
                                )
                                |> (*) 0.5

                        cxCircle : Float
                        cxCircle =
                            left + right |> (*) 0.5

                        cyCircle : Float
                        cyCircle =
                            top + bottom |> (*) 0.5
                    in
                    [ Canvas.circle ( cxCircle, cyCircle ) ray
                    , Canvas.path ( left, top )
                        [ Canvas.lineTo ( right, bottom ) ]
                    , Canvas.path ( right, top )
                        [ Canvas.lineTo ( left, bottom ) ]
                    ]

        renderedBboxBottomLeft : List Canvas.Shape
        renderedBboxBottomLeft =
            case bboxesDrawn.bottomLeft of
                Nothing ->
                    []

                Just { left, top, right, bottom } ->
                    let
                        bboxWidth : Float
                        bboxWidth =
                            right - left

                        bboxHeight : Float
                        bboxHeight =
                            bottom - top

                        ray : Float
                        ray =
                            sqrt
                                ((bboxWidth * bboxWidth)
                                    + (bboxHeight * bboxHeight)
                                )
                                |> (*) 0.5

                        cxCircle : Float
                        cxCircle =
                            left + right |> (*) 0.5

                        cyCircle : Float
                        cyCircle =
                            top + bottom |> (*) 0.5
                    in
                    [ Canvas.circle ( cxCircle, cyCircle ) ray
                    , Canvas.path ( left, top )
                        [ Canvas.lineTo ( right, bottom ) ]
                    , Canvas.path ( right, top )
                        [ Canvas.lineTo ( left, bottom ) ]
                    ]

        renderedBboxBottomRight : List Canvas.Shape
        renderedBboxBottomRight =
            case bboxesDrawn.bottomRight of
                Nothing ->
                    []

                Just { left, top, right, bottom } ->
                    let
                        bboxWidth : Float
                        bboxWidth =
                            right - left

                        bboxHeight : Float
                        bboxHeight =
                            bottom - top

                        ray : Float
                        ray =
                            sqrt
                                ((bboxWidth * bboxWidth)
                                    + (bboxHeight * bboxHeight)
                                )
                                |> (*) 0.5

                        cxCircle : Float
                        cxCircle =
                            left + right |> (*) 0.5

                        cyCircle : Float
                        cyCircle =
                            top + bottom |> (*) 0.5
                    in
                    [ Canvas.circle ( cxCircle, cyCircle ) ray
                    , Canvas.path ( left, top )
                        [ Canvas.lineTo ( right, bottom ) ]
                    , Canvas.path ( right, top )
                        [ Canvas.lineTo ( left, bottom ) ]
                    ]

        completeRenderedBbox : Canvas.Renderable
        completeRenderedBbox =
            let
                strokeWidth =
                    viewer.scale * 2
            in
            [ renderedBboxTopLeft
            , renderedBboxBottomLeft
            , renderedBboxTopRight
            , renderedBboxBottomRight
            ]
                |> List.concat
                |> Canvas.shapes
                    [ Canvas.Settings.fill (Color.rgba 1 1 1 0.3)
                    , Canvas.Settings.stroke Color.red
                    , Canvas.Settings.Line.lineWidth strokeWidth
                    , Viewer.Canvas.transform viewer
                    ]

        canvasViewer =
            Canvas.toHtml ( round viewerWidth, round viewerHeight )
                [ Html.Attributes.id "theCanvas"
                , Html.Attributes.style "display" "block"
                , Wheel.onWheel (zoomWheelMsg viewer)
                , msgOn "pointerdown" (Json.Decode.map (PointerMsg << PointerDownRaw) Json.Decode.value)
                , Pointer.onUp (\e -> PointerMsg (PointerUp e.pointer.offsetPos))
                , Html.Attributes.style "touch-action" "none"
                , Html.Events.preventDefaultOn "pointermove" <|
                    Json.Decode.map (\coords -> ( PointerMsg (PointerMove coords), True )) <|
                        Json.Decode.map2 Tuple.pair
                            (Json.Decode.field "clientX" Json.Decode.float)
                            (Json.Decode.field "clientY" Json.Decode.float)
                ]
                [ clearCanvas, renderedImage, completeRenderedBbox ]
    in
    Element.column [ height fill ]
        [ headerBar
            [ ( PageImages, True )
            , ( PageConfig, False )
            , ( PageRegistration, False )
            , ( PageLogs, False )
            , ( PageLighting, False )
            ]
        , runProgressBar model
        , Element.html <|
            Html.node "style"
                []
                [ Html.text ".pixelated { image-rendering: pixelated; image-rendering: crisp-edges; }" ]
        , Element.el
            [ Element.inFront buttonsRow
            , Element.inFront
                (Element.row [ alignBottom, width fill ]
                    [ clickButton alignLeft True ClickPreviousImage "Previous image" Icon.arrowLeftCircle
                    , clickButton alignRight True ClickNextImage "Next image" Icon.arrowRightCircle
                    ]
                )
            , Element.clip
            , height fill
            ]
            (Element.html canvasViewer)
        ]


msgOn : String -> Decoder msg -> Html.Attribute msg
msgOn event =
    Json.Decode.map (\msg -> { message = msg, stopPropagation = True, preventDefault = True })
        >> Html.Events.custom event


zoomWheelMsg : Viewer -> Wheel.Event -> Msg
zoomWheelMsg viewer event =
    let
        coordinates =
            Viewer.coordinatesAt event.mouseEvent.offsetPos viewer
    in
    if event.deltaY > 0 then
        ZoomMsg (ZoomAwayFrom coordinates)

    else
        ZoomMsg (ZoomToward coordinates)


zoomWheelRegMsg : Quadrant -> Viewer -> Wheel.Event -> Msg
zoomWheelRegMsg quadrant viewer event =
    let
        coordinates =
            Viewer.coordinatesAt event.mouseEvent.offsetPos viewer
    in
    if event.deltaY > 0 then
        ZoomMsg (ZoomAwayFromReg quadrant coordinates)

    else
        ZoomMsg (ZoomTowardReg quadrant coordinates)


viewHome : FileDraggingState -> Element Msg
viewHome draggingState =
    Element.column (padding 20 :: width fill :: height fill :: onDropAttributes)
        [ viewTitle
        , dropAndLoadArea draggingState
        ]


viewLoading : { names : Set String, loaded : Dict String Image } -> Element Msg
viewLoading { names, loaded } =
    let
        totalCount =
            Set.size names

        loadCount =
            Dict.size loaded
    in
    Element.column [ padding 20, width fill, height fill ]
        [ viewTitle
        , Element.el [ width fill, height fill ]
            (Element.column
                [ centerX, centerY, spacing 32 ]
                [ Element.el loadingBoxBorderAttributes (loadBar loadCount totalCount)
                , Element.el [ centerX ] (Element.text ("Loading " ++ String.fromInt totalCount ++ " images"))
                ]
            )
        ]


loadBar : Int -> Int -> Element msg
loadBar loaded total =
    let
        barLength =
            (325 - 2 * 4) * loaded // total
    in
    Element.el
        [ width (Element.px barLength)
        , height Element.fill
        , Element.Background.color Style.dropColor
        , Element.htmlAttribute
            (Transition.properties
                [ Transition.property "width" 200 [] ]
            )
        ]
        Element.none


viewTitle : Element msg
viewTitle =
    Element.column [ centerX, spacing 16 ]
        [ Element.paragraph [ Element.Font.center, Element.Font.size 32 ] [ Element.text "Low rank image registration" ]
        , Element.row [ alignRight, spacing 8 ]
            [ Element.link [ Element.Font.underline ]
                { url = "https://github.com/mpizenberg/lowrr", label = Element.text "code on GitHub" }
            , Element.el [] Element.none
            , Icon.github 16
            ]
        , Element.row [ alignRight, spacing 8 ]
            [ Element.link [ Element.Font.underline ]
                { url = "https://hal.archives-ouvertes.fr/hal-03172399", label = Element.text "read the paper" }
            , Element.el [] Element.none
            , Icon.fileText 16
            ]
        ]


dropAndLoadArea : FileDraggingState -> Element Msg
dropAndLoadArea draggingState =
    let
        borderStyle =
            case draggingState of
                Idle ->
                    Element.Border.dashed

                DraggingSomeFiles ->
                    Element.Border.solid

        dropOrLoadText =
            Element.row [ centerX ]
                [ Element.text "Drop images or "
                , Element.html
                    (File.hiddenInputMultiple
                        "TheFileInput"
                        [ "image/*" ]
                        (\file otherFiles -> DragDropMsg (Drop file otherFiles))
                    )
                , Element.el [ Element.Font.underline ]
                    (Element.html
                        (Html.label [ Html.Attributes.for "TheFileInput", Html.Attributes.style "cursor" "pointer" ]
                            [ Html.text "load from disk" ]
                        )
                    )
                ]

        useDirectlyProvided =
            Element.paragraph [ centerX, Element.Font.center ]
                [ Element.text "You can also directly use "
                , Element.Input.button [ Element.Font.underline ]
                    { onPress =
                        Just
                            (LoadExampleImages
                                [ "/img/bd_caen/01.jpg"
                                , "/img/bd_caen/02.jpg"
                                , "/img/bd_caen/03.jpg"
                                , "/img/bd_caen/04.jpg"
                                , "/img/bd_caen/05.jpg"
                                , "/img/bd_caen/06.jpg"
                                ]
                            )
                    , label = Element.text "this example set of 6 images"
                    }
                ]
    in
    Element.el [ width fill, height fill ]
        (Element.column [ centerX, centerY, spacing 32 ]
            [ Element.el (dropIconBorderAttributes borderStyle) (Icon.arrowDown 48)
            , dropOrLoadText
            , useDirectlyProvided
            ]
        )


dropIconBorderAttributes : Element.Attribute msg -> List (Element.Attribute msg)
dropIconBorderAttributes dashedAttribute =
    [ Element.Border.width 4
    , Element.Font.color Style.dropColor
    , centerX
    , Element.clip

    -- below is different
    , paddingXY 16 16
    , dashedAttribute
    , Element.Border.rounded 16
    , height (Element.px (48 + (16 + 4) * 2))
    , width (Element.px (48 + (16 + 4) * 2))
    , borderTransition
    ]


loadingBoxBorderAttributes : List (Element.Attribute msg)
loadingBoxBorderAttributes =
    [ Element.Border.width 4
    , Element.Font.color Style.dropColor
    , centerX
    , Element.clip

    -- below is different
    , paddingXY 0 0
    , Element.Border.solid
    , Element.Border.rounded 0
    , height (Element.px ((16 + 4) * 2))
    , width (Element.px 325)
    , borderTransition
    ]


borderTransition : Element.Attribute msg
borderTransition =
    Element.htmlAttribute
        (Transition.properties
            [ Transition.property "border-radius" 300 []
            , Transition.property "height" 300 []
            , Transition.property "width" 300 []
            ]
        )


onDropAttributes : List (Element.Attribute Msg)
onDropAttributes =
    List.map Element.htmlAttribute
        (File.onDrop
            { onOver = \file otherFiles -> DragDropMsg (DragOver file otherFiles)
            , onDrop = \file otherFiles -> DragDropMsg (Drop file otherFiles)
            , onLeave = Just { id = "FileDropArea", msg = DragDropMsg DragLeave }
            }
        )
