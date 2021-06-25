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


type alias Model =
    -- Current state of the application
    { state : State
    , device : Device
    , params : Parameters
    , paramsForm : ParametersForm
    , paramsInfo : ParametersToggleInfo
    , viewer : Viewer
    , registeredViewer : Viewer
    , pointerMode : PointerMode
    , bboxDrawn : Maybe BBox
    , registeredImages : Maybe (Pivot Image)
    , logs : List { lvl : Int, content : String }
    , verbosity : Int
    , autoscroll : Bool
    , runStep : RunStep
    , imagesCount : Int
    }


initialModel : Device.Size -> Model
initialModel size =
    { state = Home Idle
    , device = Device.classify size
    , params = defaultParams
    , paramsForm = defaultParamsForm
    , paramsInfo = defaultParamsInfo
    , viewer = Viewer.withSize ( size.width, size.height - toFloat (headerHeight + progressBarHeight) )
    , registeredViewer = Viewer.withSize ( size.width, size.height - toFloat (headerHeight + progressBarHeight) )
    , pointerMode = WaitingMove
    , bboxDrawn = Nothing
    , registeredImages = Nothing
    , logs = []
    , verbosity = 2
    , autoscroll = True
    , runStep = StepNotStarted
    , imagesCount = 0
    }



-- Params #########################################


type alias Parameters =
    { crop : Maybe Crop
    , maxVerbosity : Int
    , equalize : Bool

    -- , center : { x : Float, y : Float }
    -- , ray : Float
    }


encodeParams : Parameters -> Value
encodeParams params =
    Json.Encode.object
        [ ( "crop", encodeMaybe encodeCrop params.crop )
        ]


encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe encoder data =
    Maybe.withDefault Json.Encode.null (Maybe.map encoder data)


type alias ParametersForm =
    { crop : CropForm.State
    , maxVerbosity : NumberInput.Field Int NumberInput.IntError
    }


type alias ParametersToggleInfo =
    { crop : Bool
    , maxVerbosity : Bool
    }


defaultParams : Parameters
defaultParams =
    { crop = Nothing
    , maxVerbosity = 3
    , equalize = True
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
    }


defaultParamsInfo : ParametersToggleInfo
defaultParamsInfo =
    { crop = False
    , maxVerbosity = False
    }


type ParamsMsg
    = ToggleEqualize Bool
      -- | ...
    | ChangeMaxVerbosity String
    | ToggleCrop Bool
    | ChangeCropLeft String
    | ChangeCropTop String
    | ChangeCropRight String
    | ChangeCropBottom String


type ParamsInfoMsg
    = ToggleInfoCrop Bool
      -- | ...
    | ToggleInfoMaxVerbosity Bool



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
    | ReceiveCroppedImages (List { id : String, img : Value })
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


type PointerMsg
    = PointerDownRaw Value
    | PointerMove ( Float, Float )
    | PointerUp ( Float, Float )


type ViewImgMsg
    = SelectMovingMode
    | SelectDrawingMode
    | CropCurrentFrame



-- Navigation ######################


type NavigationMsg
    = GoToPageImages
    | GoToPageConfig
    | GoToPageLogs


type PageHeader
    = PageImages
    | PageConfig
    | PageLogs
