module Camera exposing (scene)

import Angle
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d


scene : Html msg
scene =
    Scene3d.unlit
        { entities =
            [ Scene3d.quad (Material.color Color.blue)
                (Point3d.meters -1 -1 0)
                (Point3d.meters 1 -1 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters -1 1 0)
            ]
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 5 2 3
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
        , clipDepth = Length.meters 1
        , background = Scene3d.transparentBackground
        , dimensions = ( Pixels.pixels 400, Pixels.pixels 300 )
        }
