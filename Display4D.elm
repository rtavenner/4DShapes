module Display4D exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, getW, vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
--import Html exposing (Html)

--import WebGL

mapTuple5 : (a -> b) -> ( a, a, a, a, a ) -> ( b, b, b, b, b )
mapTuple5 f (a,b,c,d,e) = (f a,f b,f c,f d,f e)

mapTuple4 : (a -> b) -> ( a, a, a, a ) -> ( b, b, b, b )
mapTuple4 f (a,b,c,d) = (f a,f b,f c,f d)

mapTuple3 : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapTuple3 f (a,b,c) = (f a,f b,f c)

mapTuple2 : (a -> b) -> ( a, a ) -> ( b, b )
mapTuple2 f (a,b) = (f a,f b)


type alias Attr4D = { pos : Vec4, color : Vec3 } 
type alias Attr3D = { pos : Vec3, color : Vec3 } 

type alias Tetrahedra = List (Attr4D,Attr4D,Attr4D,Attr4D)

mkM4 : ((Float,Float,Float,Float),(Float,Float,Float,Float),(Float,Float,Float,Float),(Float,Float,Float,Float)) -> Mat4
mkM4 ((m11,m12,m13,m14),(m21,m22,m23,m24),(m31,m32,m33,m34),(m41,m42,m43,m44)) =
    Mat4.fromRecord {m11=m11,m12=m12,m13=m13,m14=m14,m21=m21,m22=m22,m23=m23,m24=m24,m31=m31,m32=m32,m33=m33,m34=m34,m41=m41,m42=m42,m43=m43,m44=m44}

transformV4 : Mat4 -> Vec4 -> Vec4
transformV4 m v = 
    case (Mat4.toRecord m, Vec4.toRecord v) of 
        (m,v) -> 
            vec4 
                (m.m11 * v.x + m.m12 * v.y + m.m13 * v.z + m.m14 * v.w)
                (m.m21 * v.x + m.m22 * v.y + m.m23 * v.z + m.m24 * v.w)
                (m.m31 * v.x + m.m32 * v.y + m.m33 * v.z + m.m34 * v.w)
                (m.m41 * v.x + m.m42 * v.y + m.m43 * v.z + m.m44 * v.w)


map : (Vec4 -> Vec4) -> Tetrahedra -> Tetrahedra
map =
    List.map 
    << mapTuple4 
    << (\f x -> {x | pos = f x.pos})

transform : Mat4 -> Tetrahedra -> Tetrahedra
transform m t =
    map (transformV4 m) t

slice : Tetrahedra -> List (Attr3D, Attr3D, Attr3D)
slice = 
    let
        -- Intersect line with hyperplane
        i : Attr4D -> Attr4D -> Attr3D
        i u v = 
            case (Vec4.toRecord u.pos, Vec4.toRecord v.pos, Vec3.toRecord u.color, Vec3.toRecord v.color) of
                (u,v,uc,vc) ->
                    { pos =
                        vec3
                            ((u.x * v.w - v.x * u.w) / (v.w - u.w)) 
                            ((u.y * v.w - v.y * u.w) / (v.w - u.w)) 
                            ((u.z * v.w - v.z * u.w) / (v.w - u.w))
                    , color =
                        vec3 
                            ((uc.x * v.w - vc.x * u.w) / (v.w - u.w)) 
                            ((uc.y * v.w - vc.y * u.w) / (v.w - u.w)) 
                            ((uc.z * v.w - vc.z * u.w) / (v.w - u.w))
                    }
        one : Attr4D -> (Attr4D,Attr4D,Attr4D) -> List (Attr3D, Attr3D, Attr3D)
        one d (a,b,c) = [(i a d, i b d, i c d)]
        two : (Attr4D,Attr4D) -> (Attr4D,Attr4D) -> List (Attr3D, Attr3D, Attr3D)
        two (a,b) (c,d) = [(i a c, i a d, i b d), (i b d, i b c, i a c)]
    in List.concatMap 
        (\(a,b,c,d) ->
            case (getW a.pos > 0, getW b.pos > 0, getW c.pos > 0, getW d.pos > 0) of
                (False,False,False,False) -> []
                (False,False,False,True ) -> one d (a,b,c)
                (False,False,True ,False) -> one c (a,b,d)
                (False,False,True ,True ) -> two (a,b) (c,d)
                (False,True ,False,False) -> one b (a,c,d)
                (False,True ,False,True ) -> two (a,c) (b,d)
                (False,True ,True ,False) -> two (a,d) (b,c)
                (False,True ,True ,True ) -> one a (b,c,d)
                (True ,False,False,False) -> one a (b,c,d)
                (True ,False,False,True ) -> two (a,d) (b,c)
                (True ,False,True ,False) -> two (a,c) (b,d)
                (True ,False,True ,True ) -> one b (a,c,d)
                (True ,True ,False,False) -> two (a,b) (c,d)
                (True ,True ,False,True ) -> one c (a,b,d)
                (True ,True ,True ,False) -> one d (a,b,c)
                (True ,True ,True ,True ) -> []
            )

--project : List (Vec4,Vec4,Vec4,Vec4) -> Html a
--project = Debug.crash "Todo"


--if
--then
--    if
--    then
--        if
--        then
--            if
--            then
--            else
--        else
--            if
--            then
--            else
--    else
--        if
--        then
--            if
--            then
--            else
--        else
--            if
--            then
--            else
--else
--    if
--    then
--        if
--        then
--            if
--            then
--            else
--        else
--            if
--            then
--            else
--    else
--        if
--        then
--            if
--            then
--            else
--        else
--            if
--            then
--            else


