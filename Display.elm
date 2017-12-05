module Display exposing (display)

import Html
import Html.Attributes exposing (width, height, style, disabled)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Display4D exposing (..)
import WebGL exposing (Mesh, Shader)





display : ({width : Int, height : Int} , Tetrahedra , Mat4 , Float) -> Html.Html msg
display (size , object , rotationMatrix , w) =
    WebGL.toHtml
        [ width size.width
        , height size.height
        , style [ ( "display", "block" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            (WebGL.triangles
                (slice 
                    (map (Vec4.add (vec4 0 0 0 w))
                        (transform rotationMatrix object))))
            (uniforms (toFloat size.width / toFloat size.height))
        ]

type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    }


uniforms : Float -> Uniforms
uniforms aspect =
    { perspective = Mat4.makePerspective 45 aspect 0.01 100
    , camera = Mat4.makeLookAt (vec3 0 8 0) (vec3 0 0 0) (vec3 0 0 1)
    }

vertexShader : Shader Attr3D Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 pos;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * vec4(pos, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]


