-- module Model exposing (BBox, FileDraggingState, Image, Model, Msg, NavigationMsg, Parameters, ParametersForm, ParametersToggleInfo, PointerMode, RunStep, State, defaultParams, defaultParamsForm, defaultParamsInfo, encodeMaybe, encodeParams, headerHeight, initialModel, progressBarHeight)


module Model exposing (..)

import Canvas.Texture exposing (Texture)
import Crop exposing (..)
import CropForm
import Device exposing (Device)
import Dict exposing (Dict)
import FileValue as File exposing (File)
import Json.Encode exposing (Value)
import Keyboard exposing (RawKey)
import NumberInput exposing (..)
import Pivot exposing (Pivot)
import Set exposing (Set)
import Viewer exposing (Viewer)



-- CSTS ############################


{-| WARNING: this has to be kept consistent with the text size in the header
-}
headerHeight : Int
headerHeight =
    40


{-| WARNING: this has to be kept consistent with the viewer size
-}
progressBarHeight : Int
progressBarHeight =
    38


type alias CroppedCircles =
    { topLeft : Maybe BBox
    , bottomLeft : Maybe BBox
    , topRight : Maybe BBox
    , bottomRight : Maybe BBox
    }


type alias Model =
    -- Current state of the application
    { state : State
    , device : Device
    , params : Parameters
    , paramsForm : ParametersForm
    , paramsInfo : ParametersToggleInfo
    , viewer : Viewer
    , registeredViewer :
        { tl : Viewer
        , tr : Viewer
        , bl : Viewer
        , br : Viewer
        }
    , pointerMode : PointerMode
    , bboxesDrawn : CroppedCircles
    , registeredImages :
        { tl : Maybe (Pivot Image)
        , tr : Maybe (Pivot Image)
        , bl : Maybe (Pivot Image)
        , br : Maybe (Pivot Image)
        }
    , registeredCenters :
        { tl : Maybe (Pivot { x : Int, y : Int })
        , tr : Maybe (Pivot { x : Int, y : Int })
        , bl : Maybe (Pivot { x : Int, y : Int })
        , br : Maybe (Pivot { x : Int, y : Int })
        }
    , logs : List { lvl : Int, content : String }
    , verbosity : Int
    , autoscroll : Bool
    , runStep : RunStep
    , imagesCount : Int
    , currentDrawingQuadrant : Maybe Quadrant
    }


initialModel : Device.Size -> Model
initialModel size =
    { state = Home Idle
    , device = Device.classify size
    , params = defaultParams
    , paramsForm = defaultParamsForm
    , paramsInfo = defaultParamsInfo
    , viewer = Viewer.withSize ( size.width, size.height - toFloat (headerHeight + progressBarHeight) )
    , registeredViewer =
        { tl = Viewer.withSize ( size.width, size.height - toFloat (headerHeight + progressBarHeight) )
        , tr = Viewer.withSize ( size.width, size.height - toFloat (headerHeight + progressBarHeight) )
        , bl = Viewer.withSize ( size.width, size.height - toFloat (headerHeight + progressBarHeight) )
        , br = Viewer.withSize ( size.width, size.height - toFloat (headerHeight + progressBarHeight) )
        }
    , pointerMode = WaitingMove
    , bboxesDrawn =
        { topLeft = Nothing
        , bottomLeft = Nothing
        , topRight = Nothing
        , bottomRight = Nothing
        }
    , registeredImages =
        { tl = Nothing
        , tr = Nothing
        , bl = Nothing
        , br = Nothing
        }
    , registeredCenters =
        { tl = Nothing
        , tr = Nothing
        , bl = Nothing
        , br = Nothing
        }
    , logs = []
    , verbosity = 2
    , autoscroll = True
    , runStep = StepNotStarted
    , imagesCount = 0
    , currentDrawingQuadrant = Nothing
    }



-- Params #########################################


type Quadrant
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight


type alias CropSet =
    { topLeft : Maybe Crop
    , bottomLeft : Maybe Crop
    , topRight : Maybe Crop
    , bottomRight : Maybe Crop
    }


type alias Parameters =
    { crops : CropSet
    , maxVerbosity : Int
    , sigma : Float
    , maskRay : Float
    , threashold : Float
    }


encodeParams : Parameters -> Value
encodeParams params =
    Json.Encode.object
        [ ( "crops", encodeCrops params.crops )
        , ( "maxVerbosity", Json.Encode.int params.maxVerbosity )
        , ( "sigma", Json.Encode.float params.sigma )
        , ( "maskRay", Json.Encode.float params.maskRay )
        , ( "threashold", Json.Encode.float params.threashold )
        ]


encodeCrops : CropSet -> Value
encodeCrops crops =
    Json.Encode.object
        [ ( "topLeft", encodeMaybe encodeCrop crops.topLeft )
        , ( "topRight", encodeMaybe encodeCrop crops.topRight )
        , ( "bottomLeft", encodeMaybe encodeCrop crops.bottomLeft )
        , ( "bottomRight", encodeMaybe encodeCrop crops.bottomRight )
        ]


encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe encoder data =
    Maybe.withDefault Json.Encode.null (Maybe.map encoder data)


type alias ParametersForm =
    { crop : CropForm.State
    , maxVerbosity : NumberInput.Field Int NumberInput.IntError
    , sigma : NumberInput.Field Float NumberInput.FloatError
    , maskRay : NumberInput.Field Float NumberInput.FloatError
    , threashold : NumberInput.Field Float NumberInput.FloatError
    }


type alias ParametersToggleInfo =
    { crop : Bool
    , maxVerbosity : Bool
    , sigma : Bool
    , maskRay : Bool
    , threashold : Bool
    }


defaultParams : Parameters
defaultParams =
    { crops =
        { topLeft = Nothing
        , bottomLeft = Nothing
        , topRight = Nothing
        , bottomRight = Nothing
        }
    , maxVerbosity = 3
    , sigma = 1.2
    , maskRay = 0.8
    , threashold = 0.7
    }


defaultParamsForm : ParametersForm
defaultParamsForm =
    let
        anyInt =
            NumberInput.intDefault

        anyFloat =
            NumberInput.floatDefault
    in
    { crop = CropForm.withSize 1920 1080
    , maxVerbosity =
        { anyInt | min = Just 0, max = Just 4 }
            |> NumberInput.setDefaultIntValue defaultParams.maxVerbosity
    , sigma =
        { anyFloat | min = Just 0.01, max = Nothing }
            |> NumberInput.setDefaultFloatValue defaultParams.sigma
    , maskRay =
        { anyFloat | min = Just 0.0, max = Just 1.0 }
            |> NumberInput.setDefaultFloatValue defaultParams.maskRay
    , threashold =
        { anyFloat | min = Just 0.0, max = Just 1.0 }
            |> NumberInput.setDefaultFloatValue defaultParams.threashold
    }


defaultParamsInfo : ParametersToggleInfo
defaultParamsInfo =
    { crop = False
    , maxVerbosity = False
    , sigma = False
    , maskRay = False
    , threashold = False
    }


type ParamsMsg
    = ChangeMaxVerbosity String
    | ToggleCrop Bool
    | ChangeCropLeft String
    | ChangeCropTop String
    | ChangeCropRight String
    | ChangeCropBottom String
    | ChangeSigma String
    | ChangeMaskRay String
    | ChangeThreashold String


type ParamsInfoMsg
    = ToggleInfoCrop Bool
    | ToggleInfoMaxVerbosity Bool
    | ToggleInfoSigma Bool
    | ToggleInfoMaskRay Bool
    | ToggleInfoThreashold Bool



-- ##################


type RunStep
    = StepNotStarted


type alias BBox =
    { left : Float
    , top : Float
    , right : Float
    , bottom : Float
    }


type State
    = Home FileDraggingState
    | Loading { names : Set String, loaded : Dict String Image }
    | ViewImgs { images : Pivot Image }
    | Config { images : Pivot Image }
    | Registration { images : Pivot Image }
    | Logs { images : Pivot Image }


type FileDraggingState
    = Idle
    | DraggingSomeFiles


type alias Image =
    { id : String
    , texture : Texture
    , width : Int
    , height : Int
    }


type PointerMode
    = WaitingMove
    | PointerMovingFromClientCoords ( Float, Float )
    | WaitingDraw
    | PointerDrawFromOffsetAndClient ( Float, Float ) ( Float, Float )



-- Update ############################################################


type Msg
    = NoMsg
    | WindowResizes Device.Size
    | DragDropMsg DragDropMsg
    | LoadExampleImages (List String)
    | ImageDecoded { id : String, img : Value }
    | KeyDown RawKey
    | ClickPreviousImage
    | ClickNextImage
    | ZoomMsg ZoomMsg
    | ViewImgMsg ViewImgMsg
    | ParamsMsg ParamsMsg
    | ParamsInfoMsg ParamsInfoMsg
    | NavigationMsg NavigationMsg
    | PointerMsg PointerMsg
    | RunAlgorithm Parameters
    | StopRunning
    | UpdateRunStep { step : String, progress : Maybe Int }
    | Log { lvl : Int, content : String }
    | VerbosityChange Float
    | ScrollLogsToEnd
    | ToggleAutoScroll Bool
    | ReceiveCroppedImages
        { tl : List { id : String, img : Value }
        , tr : List { id : String, img : Value }
        , bl : List { id : String, img : Value }
        , br : List { id : String, img : Value }
        }
    | ReceiveCenters
        { tl : List { x : Int, y : Int }
        , tr : List { x : Int, y : Int }
        , bl : List { x : Int, y : Int }
        , br : List { x : Int, y : Int }
        }
    | SaveRegisteredImages


type DragDropMsg
    = DragOver File (List File)
    | Drop File (List File)
    | DragLeave


type ZoomMsg
    = ZoomFit Image
    | ZoomIn
    | ZoomOut
    | ZoomToward ( Float, Float )
    | ZoomAwayFrom ( Float, Float )
    | ZoomInReg Quadrant
    | ZoomOutReg Quadrant
    | ZoomFitReg Quadrant Image
    | ZoomTowardReg Quadrant ( Float, Float )
    | ZoomAwayFromReg Quadrant ( Float, Float )


type PointerMsg
    = PointerDownRaw Value
    | PointerMove ( Float, Float )
    | PointerUp ( Float, Float )


type ViewImgMsg
    = SelectMovingMode
    | SelectDrawingMode



-- Navigation ######################


type NavigationMsg
    = GoToPageImages
    | GoToPageConfig
    | GoToPageRegistration
    | GoToPageLogs


type PageHeader
    = PageImages
    | PageConfig
    | PageRegistration
    | PageLogs
