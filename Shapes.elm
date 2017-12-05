module Shapes exposing (Shape(..),Color(..),Border,shape)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Display4D exposing (..)

import Random

randomColors : List Vec3
randomColors =
    Random.step 
        (Random.list 600 
            (Random.map3 vec3 
                (Random.float 0 1) 
                (Random.float 0 1) 
                (Random.float 0 1)))
        (Random.initialSeed 123456789)
    |> Tuple.first


type Shape = C5 | C8 | C16 | C24 | C120 | C600
type Color = None | Random | Pattern
type alias Border = Bool

type alias Pair a = (a,a)

type Cell 
    = Tetrahedron  (Attr4D, Attr4D, Attr4D, Attr4D)
    | Cube         (Pair (Pair (Pair Attr4D)))
    | Octahedron   (Pair Attr4D, Pair Attr4D, Pair Attr4D)
    | Dodecahedron (Pair (Pair Attr4D, Pair Attr4D, Pair Attr4D, Pair Attr4D, Pair Attr4D))
    | TriPrism     (Pair (Attr4D, Attr4D, Attr4D))
    | PentPrism    (Pair (Attr4D, Attr4D, Attr4D, Attr4D, Attr4D))


cellToTetrahedra : Cell -> Tetrahedra
cellToTetrahedra c =
    case c of
        Tetrahedron t -> [t]
        Cube (((p000,p001),(p010,p011)),((p100,p101),(p110,p111))) ->
            [ (p000,p011,p101,p110)
            , (p001,p101,p011,p000)
            , (p010,p110,p000,p011)
            , (p100,p000,p110,p101)
            , (p111,p011,p101,p110)
            ]
        Octahedron ((a1,a2),(b1,b2),(c1,c2)) ->
            [ (a1,a2,b1,c1)
            , (a1,a2,b1,c2)
            , (a1,a2,b2,c2)
            , (a1,a2,b2,c1)
            ]
        TriPrism ((a1,b1,c1),(a2,b2,c2)) ->
            [ (a1,b1,c1,c2)
            , (a1,b1,b2,c2)
            , (a1,a2,b2,c2)
            ]
        Dodecahedron (((a,b),(c,d),(e,f),(g,h),(i,j)),((k,l),(m,n),(o,p),(q,r),(s,t))) -> 
            List.concatMap cellToTetrahedra 
                [ Cube (((c,g),(b,j)),((k,s),(n,r)))
                , TriPrism ((c,b,a),(g,j,i))
                , TriPrism ((c,k,d),(b,n,m))
                , TriPrism ((c,g,e),(k,s,f))
                , TriPrism ((r,s,t),(n,k,l))
                , TriPrism ((r,j,q),(s,g,h))
                , TriPrism ((r,n,p),(j,b,o))
                ]
        PentPrism ((a,b,c,d,e),(f,g,h,i,j)) -> 
            List.concatMap cellToTetrahedra 
                [ Cube (((a,b),(d,c)),((f,g),(i,h)))
                , TriPrism ((a,d,e),(f,i,j))
                ]


mapCell : (Attr4D -> Attr4D) -> Cell -> Cell
mapCell f c =
    case c of
        Tetrahedron t -> Tetrahedron <| 
            mapTuple4 f t
        Cube c -> Cube <| 
            (mapTuple2 << mapTuple2 << mapTuple2) f c
        Octahedron o -> Octahedron <| 
            (mapTuple3 << mapTuple2) f o
        Dodecahedron d -> Dodecahedron <| 
            (mapTuple2 << mapTuple5 << mapTuple2) f d
        TriPrism p -> TriPrism <| 
            (mapTuple2 << mapTuple3) f p
        PentPrism p -> PentPrism <| 
            (mapTuple2 << mapTuple5) f p

recolor : Vec3 -> Cell -> Cell
recolor c = mapCell (\x -> { x | color = c })

avg : List Vec4 -> Vec4
avg l = 
    Vec4.scale 
        (1 / toFloat (List.length l)) 
        (List.foldr Vec4.add (vec4 0 0 0 0) l)

addBorder : Cell -> List Cell
addBorder c = 
    let color col x = {pos = x, color = col}
        black = color (vec3 0 0 0)
        nudge pos center = Vec4.add (Vec4.scale 0.9 pos) (Vec4.scale 0.1 center)
    in case c of
        Tetrahedron (a,b,c,d) ->
            let center = avg [a.pos,b.pos,c.pos,d.pos]
                a_ = nudge a.pos center
                b_ = nudge b.pos center
                c_ = nudge c.pos center
                d_ = nudge d.pos center
                tri =
                    TriPrism << mapTuple2 (mapTuple3 black)
            in 
                [ Tetrahedron
                    ( color a.color a_
                    , color b.color b_
                    , color c.color c_
                    , color d.color d_
                    )
                , tri ((a.pos,b.pos,c.pos),(a_,b_,c_))
                , tri ((b.pos,c.pos,d.pos),(b_,c_,d_))
                , tri ((c.pos,d.pos,a.pos),(c_,d_,a_))
                , tri ((d.pos,a.pos,b.pos),(d_,a_,b_))
                ]
        Cube (((p000,p001),(p010,p011)),((p100,p101),(p110,p111))) ->
            let center = avg [ p000.pos, p001.pos, p010.pos, p011.pos, p100.pos, p101.pos, p110.pos, p111.pos]
                p000_ = nudge p000.pos center
                p001_ = nudge p001.pos center
                p010_ = nudge p010.pos center
                p011_ = nudge p011.pos center
                p100_ = nudge p100.pos center
                p101_ = nudge p101.pos center
                p110_ = nudge p110.pos center
                p111_ = nudge p111.pos center
            in 
            Cube 
                ( ( ( color p000.color p000_
                    , color p001.color p001_)
                  , ( color p010.color p010_
                    , color p011.color p011_))
                , ( ( color p100.color p100_
                    , color p101.color p101_)
                  , ( color p110.color p110_
                    , color p111.color p111_))
                )
            :: List.map (Cube << (mapTuple2 <| mapTuple2 <| mapTuple2 black))
                [ (((p000.pos,p000_),(p001.pos,p001_)),((p010.pos,p010_),(p011.pos,p011_)))
                , (((p100.pos,p100_),(p101.pos,p101_)),((p110.pos,p110_),(p111.pos,p111_)))
                , (((p000.pos,p000_),(p010.pos,p010_)),((p100.pos,p100_),(p110.pos,p110_)))
                , (((p001.pos,p001_),(p011.pos,p011_)),((p101.pos,p101_),(p111.pos,p111_)))
                , (((p000.pos,p000_),(p100.pos,p100_)),((p001.pos,p001_),(p101.pos,p101_)))
                , (((p010.pos,p010_),(p110.pos,p110_)),((p011.pos,p011_),(p111.pos,p111_)))
                ]
        Octahedron ((a1,a2),(b1,b2),(c1,c2)) ->
            let center = avg [ a1.pos, a2.pos, b1.pos, b2.pos, c1.pos, c2.pos]
                a1_ = nudge a1.pos center
                a2_ = nudge a2.pos center
                b1_ = nudge b1.pos center
                b2_ = nudge b2.pos center
                c1_ = nudge c1.pos center
                c2_ = nudge c2.pos center
            in 
            Octahedron
                ( ( color a1.color a1_
                  , color a2.color a2_)
                , ( color b1.color b1_
                  , color b2.color b2_)
                , ( color c1.color c1_
                  , color c2.color c2_))
            :: List.map 
                (mapTuple3 (mapTuple2 black) >> (\((a1,a2),(b1,b2),(c1,c2)) -> ((a1,b1,c1),(a2,b2,c2))) >> TriPrism) 
                [ ((a1.pos,a1_),(b1.pos,b1_),(c1.pos,c1_))
                , ((a1.pos,a1_),(b1.pos,b1_),(c2.pos,c2_))
                , ((a1.pos,a1_),(b2.pos,b2_),(c1.pos,c1_))
                , ((a1.pos,a1_),(b2.pos,b2_),(c2.pos,c2_))
                , ((a2.pos,a2_),(b1.pos,b1_),(c1.pos,c1_))
                , ((a2.pos,a2_),(b1.pos,b1_),(c2.pos,c2_))
                , ((a2.pos,a2_),(b2.pos,b2_),(c1.pos,c1_))
                , ((a2.pos,a2_),(b2.pos,b2_),(c2.pos,c2_))
                ]
        Dodecahedron (((a,b),(c,d),(e,f),(g,h),(i,j)),((k,l),(m,n),(o,p),(q,r),(s,t))) ->
            let center = avg <| List.map .pos [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t]
                a_ = nudge a.pos center
                b_ = nudge b.pos center
                c_ = nudge c.pos center
                d_ = nudge d.pos center
                e_ = nudge e.pos center
                f_ = nudge f.pos center
                g_ = nudge g.pos center
                h_ = nudge h.pos center
                i_ = nudge i.pos center
                j_ = nudge j.pos center
                k_ = nudge k.pos center
                l_ = nudge l.pos center
                m_ = nudge m.pos center
                n_ = nudge n.pos center
                o_ = nudge o.pos center
                p_ = nudge p.pos center
                q_ = nudge q.pos center
                r_ = nudge r.pos center
                s_ = nudge s.pos center
                t_ = nudge t.pos center
            in 
            Dodecahedron
                (((color a.color a_
                  ,color b.color b_)
                 ,(color c.color c_
                  ,color d.color d_)
                 ,(color e.color e_
                  ,color f.color f_)
                 ,(color g.color g_
                  ,color h.color h_)
                 ,(color i.color i_
                  ,color j.color j_))
                ,((color k.color k_
                  ,color l.color l_)
                 ,(color m.color m_
                  ,color n.color n_)
                 ,(color o.color o_
                  ,color p.color p_)
                 ,(color q.color q_
                  ,color r.color r_)
                 ,(color s.color s_
                  ,color t.color t_)))
            :: List.map (PentPrism << (mapTuple2 <| mapTuple5 black))
                [ ((a.pos,c.pos,e.pos,g.pos,i.pos),(a_,c_,e_,g_,i_))
                , ((a.pos,b.pos,m.pos,d.pos,c.pos),(a_,b_,m_,d_,c_))
                , ((c.pos,d.pos,k.pos,f.pos,e.pos),(c_,d_,k_,f_,e_))
                , ((e.pos,f.pos,s.pos,h.pos,g.pos),(e_,f_,s_,h_,g_))
                , ((g.pos,h.pos,q.pos,j.pos,i.pos),(g_,h_,q_,j_,i_))
                , ((i.pos,j.pos,o.pos,b.pos,a.pos),(i_,j_,o_,b_,a_))
                , ((t.pos,r.pos,p.pos,n.pos,l.pos),(t_,r_,p_,n_,l_))
                , ((t.pos,s.pos,h.pos,q.pos,r.pos),(t_,s_,h_,q_,r_))
                , ((r.pos,q.pos,j.pos,o.pos,p.pos),(r_,q_,j_,o_,p_))
                , ((p.pos,o.pos,b.pos,m.pos,n.pos),(p_,o_,b_,m_,n_))
                , ((n.pos,m.pos,d.pos,k.pos,l.pos),(n_,m_,d_,k_,l_))
                , ((l.pos,k.pos,f.pos,s.pos,t.pos),(l_,k_,f_,s_,t_))
                ]
        x -> [x]

shape : Shape -> Color -> Border -> Tetrahedra
shape shape color border =
    (case shape of
        C5   -> simplex
        C8   -> tesseract
        C16  -> sixteenCell
        C24  -> twentyFourCell
        C120 -> oneHundredTwentyCell
        C600 -> sixHundredCell
    )
    |> (case color of
        None -> List.map (recolor (vec3 0.8 0.8 0.8))
        Random -> List.map2 recolor randomColors
        Pattern -> identity)
    |> (if border then List.concatMap addBorder else identity)
    |> List.concatMap cellToTetrahedra



phi : Float
phi = (sqrt 5 + 1) / 2



tetrahedron : Vec3 -> Vec4 -> Vec4 -> Vec4 -> Vec4 -> Cell
tetrahedron color a b c d =
    Tetrahedron
        ( {pos = a, color = color}
        , {pos = b, color = color}
        , {pos = c, color = color}
        , {pos = d, color = color}
        )


cube : Vec3 -> Vec4 -> (Vec4, Vec4, Vec4) -> Cell
cube color center (x,y,z) =
    Cube
        ((({ pos = Vec4.add (Vec4.add center x) (Vec4.add y z), color = color}
          ,{ pos = Vec4.add (Vec4.add center x) (Vec4.sub y z), color = color})
         ,({ pos = Vec4.sub (Vec4.add center x) (Vec4.sub y z), color = color}
          ,{ pos = Vec4.sub (Vec4.add center x) (Vec4.add y z), color = color}))
        ,(({ pos = Vec4.add (Vec4.sub center x) (Vec4.add y z), color = color}
          ,{ pos = Vec4.add (Vec4.sub center x) (Vec4.sub y z), color = color})
         ,({ pos = Vec4.sub (Vec4.sub center x) (Vec4.sub y z), color = color}
          ,{ pos = Vec4.sub (Vec4.sub center x) (Vec4.add y z), color = color})))


octahedron : Vec3 -> (Vec4,Vec4) -> (Vec4,Vec4) -> (Vec4,Vec4) -> Cell
octahedron color (a1,a2) (b1,b2) (c1,c2) =
    let a1_ = {pos = a1, color = color}
        b1_ = {pos = b1, color = color}
        c1_ = {pos = c1, color = color}
        a2_ = {pos = a2, color = color}
        b2_ = {pos = b2, color = color}
        c2_ = {pos = c2, color = color}
    in 
    Octahedron ((a1_,a2_),(b1_,b2_),(c1_,c2_))


dodeca : Vec3 -> Vec4 -> (Vec4,Vec4,Vec4) -> Cell
dodeca color center (x,y,z) = 
    let v3 a b c = 
        Vec4.add 
            (Vec4.add 
                center 
                (Vec4.scale a x)) 
            (Vec4.add 
                (Vec4.scale b y) 
                (Vec4.scale c z))
    in
    ( ( (v3 ( 1/phi) 0      phi , v3  1   -1        1     )
      , (v3   1      1    1     , v3  phi (1/phi)   0     )
      , (v3   0      phi (1/phi), v3  0      phi  (-1/phi))
      , (v3  -1      1    1     , v3 -phi (1/phi)   0     )
      , (v3 (-1/phi) 0      phi , v3 -1   -1        1     ))
    , ( (v3  1     1      -1     , v3 ( 1/phi)  0      -phi )
      , (v3  phi (-1/phi)  0     , v3   1      -1    -1     )
      , (v3  0      -phi  (1/phi), v3   0      -phi (-1/phi))
      , (v3 -phi (-1/phi)  0     , v3  -1      -1    -1     )
      , (v3 -1     1      -1     , v3 (-1/phi)  0      -phi )))
    |> (mapTuple2 << mapTuple5 << mapTuple2) (\x -> {pos = x, color = color})
    |> Dodecahedron 



tesseract : List Cell
tesseract =
    [ cube (vec3 0.4 0.4 0.4) (vec4  1 0 0 0) (vec4 0 1 0 0, vec4 0 0 1 0, vec4 0 0 0 1)
    , cube (vec3 0.4 0.4 0.4) (vec4 -1 0 0 0) (vec4 0 1 0 0, vec4 0 0 1 0, vec4 0 0 0 1)
    , cube (vec3 0.4 0.4 0.5) (vec4 0  1 0 0) (vec4 1 0 0 0, vec4 0 0 1 0, vec4 0 0 0 1)
    , cube (vec3 0.4 0.4 0.5) (vec4 0 -1 0 0) (vec4 1 0 0 0, vec4 0 0 1 0, vec4 0 0 0 1)
    , cube (vec3 0.8 0.8 0.7) (vec4 0 0  1 0) (vec4 1 0 0 0, vec4 0 1 0 0, vec4 0 0 0 1)
    , cube (vec3 0.8 0.8 0.7) (vec4 0 0 -1 0) (vec4 1 0 0 0, vec4 0 1 0 0, vec4 0 0 0 1)
    , cube (vec3 0.8 0.8 0.8) (vec4 0 0 0  1) (vec4 1 0 0 0, vec4 0 1 0 0, vec4 0 0 1 0)
    , cube (vec3 0.8 0.8 0.8) (vec4 0 0 0 -1) (vec4 1 0 0 0, vec4 0 1 0 0, vec4 0 0 1 0)
    ]

simplex : List Cell
simplex =
    let x = (2 + phi) / 5
        p1 = Vec4.sub (vec4   2   0   0   0) (vec4 x x x x)
        p2 = Vec4.sub (vec4   0   2   0   0) (vec4 x x x x)
        p3 = Vec4.sub (vec4   0   0   2   0) (vec4 x x x x)
        p4 = Vec4.sub (vec4   0   0   0   2) (vec4 x x x x)
        p5 = Vec4.sub (vec4 phi phi phi phi) (vec4 x x x x)
        f x = (cos (2*pi*x/15) + 1) / 2
    in 
        [ tetrahedron (vec3 (f 0) (f 5) (f 10)) p1 p2 p3 p4
        , tetrahedron (vec3 (f 1) (f 6) (f 11)) p2 p3 p4 p5
        , tetrahedron (vec3 (f 2) (f 7) (f 12)) p3 p4 p5 p1
        , tetrahedron (vec3 (f 3) (f 8) (f 13)) p4 p5 p1 p2
        , tetrahedron (vec3 (f 4) (f 9) (f 14)) p5 p1 p2 p3
        ]

sixteenCell : List Cell
sixteenCell =
    let p1 = vec4 2 0 0 0
        p2 = vec4 0 2 0 0
        p3 = vec4 0 0 2 0
        p4 = vec4 0 0 0 2
        p5 = vec4 0 0 0 -2
        p6 = vec4 0 0 -2 0
        p7 = vec4 0 -2 0 0
        p8 = vec4 -2 0 0 0
    in 
    [ tetrahedron (vec3 0.8 0.8 0.8) p1 p2 p3 p4
    , tetrahedron (vec3 0.6 0.6 0.6) p1 p2 p3 p5
    , tetrahedron (vec3 0.3 0.3 0.3) p1 p2 p6 p4
    , tetrahedron (vec3 0.8 0.8 0.8) p1 p2 p6 p5
    , tetrahedron (vec3 0.3 0.3 0.3) p1 p7 p3 p4
    , tetrahedron (vec3 0.1 0.1 0.1) p1 p7 p3 p5
    , tetrahedron (vec3 0.1 0.1 0.1) p1 p7 p6 p4
    , tetrahedron (vec3 0.6 0.6 0.6) p1 p7 p6 p5
    , tetrahedron (vec3 0.6 0.6 0.6) p8 p2 p3 p4
    , tetrahedron (vec3 0.1 0.1 0.1) p8 p2 p3 p5
    , tetrahedron (vec3 0.1 0.1 0.1) p8 p2 p6 p4
    , tetrahedron (vec3 0.3 0.3 0.3) p8 p2 p6 p5
    , tetrahedron (vec3 0.8 0.8 0.8) p8 p7 p3 p4
    , tetrahedron (vec3 0.3 0.3 0.3) p8 p7 p3 p5
    , tetrahedron (vec3 0.6 0.6 0.6) p8 p7 p6 p4
    , tetrahedron (vec3 0.8 0.8 0.8) p8 p7 p6 p5
    ]

twentyFourCell : List Cell
twentyFourCell =
    let add4 a b c d = Vec4.add (Vec4.add a b) (Vec4.add c d)
        oct color (side1,side2) (main1, main2) =
            octahedron color 
                (Vec4.scale 2 main1, Vec4.scale 2 main2) 
                (add4 main1 main2 side1 side2, add4 main1 main2 (Vec4.negate side1) (Vec4.negate side2))
                (add4 main1 main2 (Vec4.negate side1) side2, add4 main1 main2 side1 (Vec4.negate side2))

        withNegatives (u,v) = [(u,v), (Vec4.negate u,v), (u,Vec4.negate v), (Vec4.negate u,Vec4.negate v)]

        apply6 f u =
            let v = mapTuple3 (f << Vec4.toTuple) u
                w = mapTuple3 f v
                x = mapTuple3 f w
                y = mapTuple3 f x
                z = mapTuple3 f y
            in u :: List.map (mapTuple3 Vec4.fromTuple) [v,w,x,y,z]
    in
    List.concat
        [ List.map (\(a,b,c) -> oct (vec3 0.8 0.4 0.4) (vec4 0 0 0 1, a) (b,c))
            (apply6 
                (\(a,b,c,d)->(b,-c,a,d)) 
                (vec4 0 0 1 0, vec4 1 0 0 0, vec4 0 1 0 0))
        , List.map (\(a,b,c) -> oct (vec3 0.4 0.8 0.4) (vec4 1 0 0 0, a) (b,c))
            (apply6 
                (\(a,b,c,d)->(a,c,d,-b))
                (vec4 0 0 0 1, vec4  0 0 1 0, vec4  0 1 0 0))
        , List.map (\(a,b,c) -> oct (vec3 0.4 0.4 0.8) (vec4 0 0 1 0, a) (b,c))
            (apply6 
                (\(a,b,c,d)->(-b,d,c,a))
                (vec4 0 1 0 0, vec4 0 0 0 1, vec4 1 0 0 0))
        , List.map (\(a,b,c) -> oct (vec3 0.8 0.8 0.8) (vec4 0 1 0 0, a) (b,c))
            (apply6 
                (\(a,b,c,d)->(-c,b,-d,-a))
                (vec4 0 0 0 1, vec4 -1 0 0 0, vec4  0 0 1 0))
        ]


oneHundredTwentyCell : List Cell
oneHundredTwentyCell = 
    let apply10 f q =
            let r = f q
                s = f r
                t = f s
                u = f t
                v = f u
                w = f v
                x = f w
                y = f x
                z = f y
            in [q,r,s,t,u,v,w,x,y,z]
        f = mapCell (\x -> {x | pos = case Vec4.toRecord x.pos of {x,y,z,w} -> vec4 (x*phi/2-y/2+z/(2*phi)+w*0) (x/2+y*phi/2+z*0+w/(2*phi)) (-x/(2*phi)+y*0+z*phi/2+w/2) (x*0-y/(2*phi)-z/2+w*phi/2)})
    in 

    List.concatMap (apply10 f) 
        [ dodeca (vec3 1 0.5 0.8) (vec4 (phi + 1) 0 0 0) (vec4 0 (phi-1) 0 0, vec4 0 0 0 (phi-1), vec4 0 0 (phi-1) 0)
        , dodeca (vec3 1 0.5 0.2) (vec4 0 (phi + 1) 0 0) (vec4 (phi-1) 0 0 0, vec4 0 0 (phi-1) 0, vec4 0 0 0 (phi-1))
        , dodeca (vec3 0 0.5 0.8) (vec4 0 0 (phi + 1) 0) (vec4 0 0 0 (phi-1), vec4 0 (phi-1) 0 0, vec4 (phi-1) 0 0 0)
        , dodeca (vec3 0 0.5 0.2) (vec4 0 0 0 (phi + 1)) (vec4 0 0 (phi-1) 0, vec4 (phi-1) 0 0 0, vec4 0 (phi-1) 0 0)
        
        , dodeca (vec3 0.5 0.8 0) 
            (vec4 ((phi + 1) / 2) ((phi + 1) / 2) ((phi + 1) / 2) ((phi + 1) / 2)) 
            ( vec4 ((phi-1)/2) -((phi-1)/2) -((phi-1)/2) ((phi-1)/2)
            , vec4 -((phi-1)/2) ((phi-1)/2) -((phi-1)/2) ((phi-1)/2)
            , vec4 -((phi-1)/2) -((phi-1)/2) ((phi-1)/2) ((phi-1)/2)
            )
        , dodeca (vec3 0.2 0 0.5) 
            (vec4 -((phi + 1) / 2) ((phi + 1) / 2) ((phi + 1) / 2) ((phi + 1) / 2)) 
            ( vec4 -((phi-1)/2) -((phi-1)/2) -((phi-1)/2) ((phi-1)/2)
            , vec4 ((phi-1)/2) ((phi-1)/2) -((phi-1)/2) ((phi-1)/2)
            , vec4 ((phi-1)/2) -((phi-1)/2) ((phi-1)/2) ((phi-1)/2)
            )
        , dodeca (vec3 0.8 1 0.5) 
            (vec4 ((phi + 1) / 2) -((phi + 1) / 2) ((phi + 1) / 2) ((phi + 1) / 2)) 
            ( vec4 ((phi-1)/2) ((phi-1)/2) -((phi-1)/2) ((phi-1)/2)
            , vec4 -((phi-1)/2) -((phi-1)/2) -((phi-1)/2) ((phi-1)/2)
            , vec4 -((phi-1)/2) ((phi-1)/2) ((phi-1)/2) ((phi-1)/2)
            )
        , dodeca (vec3 0.5 0.8 1) 
            (vec4 -((phi + 1) / 2) -((phi + 1) / 2) ((phi + 1) / 2) ((phi + 1) / 2)) 
            ( vec4 -((phi-1)/2) ((phi-1)/2) -((phi-1)/2) ((phi-1)/2)
            , vec4 ((phi-1)/2) -((phi-1)/2) -((phi-1)/2) ((phi-1)/2)
            , vec4 ((phi-1)/2) ((phi-1)/2) ((phi-1)/2) ((phi-1)/2)
            )

        , dodeca (vec3 0.8 0 0.5) 
            (vec4 ((phi + 1) / 2) ((phi + 1) / 2) -((phi + 1) / 2) ((phi + 1) / 2)) 
            ( vec4 ((phi-1)/2) -((phi-1)/2) ((phi-1)/2) ((phi-1)/2)
            , vec4 -((phi-1)/2) ((phi-1)/2) ((phi-1)/2) ((phi-1)/2)
            , vec4 -((phi-1)/2) -((phi-1)/2) -((phi-1)/2) ((phi-1)/2)
            )
        , dodeca (vec3 0.5 0.2 1) 
            (vec4 -((phi + 1) / 2) ((phi + 1) / 2) -((phi + 1) / 2) ((phi + 1) / 2)) 
            ( vec4 -((phi-1)/2) -((phi-1)/2) ((phi-1)/2) ((phi-1)/2)
            , vec4 ((phi-1)/2) ((phi-1)/2) ((phi-1)/2) ((phi-1)/2)
            , vec4 ((phi-1)/2) -((phi-1)/2) -((phi-1)/2) ((phi-1)/2)
            )
        , dodeca (vec3 0.5 0.2 0) 
            (vec4 ((phi + 1) / 2) -((phi + 1) / 2) -((phi + 1) / 2) ((phi + 1) / 2)) 
            ( vec4 ((phi-1)/2) ((phi-1)/2) ((phi-1)/2) ((phi-1)/2)
            , vec4 -((phi-1)/2) -((phi-1)/2) ((phi-1)/2) ((phi-1)/2)
            , vec4 -((phi-1)/2) ((phi-1)/2) -((phi-1)/2) ((phi-1)/2)
            )
        , dodeca (vec3 0.2 1 0.5) 
            (vec4 -((phi + 1) / 2) -((phi + 1) / 2) -((phi + 1) / 2) ((phi + 1) / 2)) 
            ( vec4 -((phi-1)/2) ((phi-1)/2) ((phi-1)/2) ((phi-1)/2)
            , vec4 ((phi-1)/2) -((phi-1)/2) ((phi-1)/2) ((phi-1)/2)
            , vec4 ((phi-1)/2) ((phi-1)/2) -((phi-1)/2) ((phi-1)/2)
            )
        ]



sixHundredCell : List Cell
sixHundredCell = 
    let apply10 f q =
            let r = f q
                s = f r
                t = f s
                u = f t
                v = f u
                w = f v
                x = f w
                y = f x
                z = f y
            in [q,r,s,t,u,v,w,x,y,z]
        f = mapCell (\x -> {x | pos = case Vec4.toRecord x.pos of {x,y,z,w} -> vec4 (x*phi/2-y/2+z/(2*phi)+w*0) (x/2+y*phi/2+z*0+w/(2*phi)) (-x/(2*phi)+y*0+z*phi/2+w/2) (x*0-y/(2*phi)-z/2+w*phi/2)})
    in 
    -- 0.618 | 0.309
    -- 0.382 | 0.191
    -- 0.236 | 0.118
    -- 0.146 | 0.073
    -- 0.090 | 0.045
    List.concatMap (apply10 f) 
        [ tetrahedron (vec3 0.309 0.191 0.118) (vec4 0 0 0 2) (vec4  -1       0 -(1/phi) phi) (vec4 -(1/phi)   1       0 phi) (vec4   0       (1/phi)  -1      phi)
        , tetrahedron (vec3 0.118 0.309 0.191) (vec4 0 0 0 2) (vec4  -1       0 -(1/phi) phi) (vec4 -(1/phi)   1       0 phi) (vec4  -1        0       (1/phi) phi)
        , tetrahedron (vec3 0.191 0.118 0.309) (vec4 0 0 0 2) (vec4  -1       0  (1/phi) phi) (vec4 -(1/phi)   1       0 phi) (vec4   0       (1/phi)   1      phi)

        , tetrahedron (vec3 0.000 0.382 0.618) (vec4 0 0 0 2) (vec4  -1       0  (1/phi) phi) (vec4 -(1/phi)  -1       0 phi) (vec4   0      -(1/phi)   1      phi)
        , tetrahedron (vec3 0.073 0.191 0.500) (vec4 0 0 0 2) (vec4  -1       0 -(1/phi) phi) (vec4 -(1/phi)  -1       0 phi) (vec4   0      -(1/phi)  -1      phi)
        , tetrahedron (vec3 0.000 0.382 0.382) (vec4 0 0 0 2) (vec4  -1       0 -(1/phi) phi) (vec4 -(1/phi)  -1       0 phi) (vec4  -1        0       (1/phi) phi)

        , tetrahedron (vec3 0.118 0.309 0.809) (vec4 0 0 0 2) (vec4  (1/phi) -1   0      phi) (vec4   0      -(1/phi)  1 phi) (vec4 -(1/phi)  -1        0      phi)
        , tetrahedron (vec3 0.191 0.118 0.691) (vec4 0 0 0 2) (vec4  (1/phi) -1   0      phi) (vec4   0      -(1/phi) -1 phi) (vec4 -(1/phi)  -1        0      phi)
        , tetrahedron (vec3 0.309 0.191 0.882) (vec4 0 0 0 2) (vec4  (1/phi) -1   0      phi) (vec4   0      -(1/phi)  1 phi) (vec4   1        0       (1/phi) phi)

        , tetrahedron (vec3 0.382 0.000 0.618) (vec4 0 0 0 2) (vec4  (1/phi)  1   0      phi) (vec4   0       (1/phi)  1 phi) (vec4   1        0       (1/phi) phi)
        , tetrahedron (vec3 0.500 0.073 0.809) (vec4 0 0 0 2) (vec4   1       0 -(1/phi) phi) (vec4  (1/phi)  -1       0 phi) (vec4   1        0       (1/phi) phi)
        , tetrahedron (vec3 0.618 0.000 0.618) (vec4 0 0 0 2) (vec4   1       0 -(1/phi) phi) (vec4  (1/phi)   1       0 phi) (vec4   1        0       (1/phi) phi)

        , tetrahedron (vec3 0.382 0.000 0.382) (vec4 0 0 0 2) (vec4  (1/phi)  1   0      phi) (vec4   0       (1/phi)  1 phi) (vec4 -(1/phi)   1        0      phi)
        , tetrahedron (vec3 0.500 0.073 0.382) (vec4 0 0 0 2) (vec4 -(1/phi)  1   0      phi) (vec4   0       (1/phi) -1 phi) (vec4  (1/phi)   1        0      phi)
        , tetrahedron (vec3 0.618 0.000 0.382) (vec4 0 0 0 2) (vec4   1       0 -(1/phi) phi) (vec4  (1/phi)   1       0 phi) (vec4   0       (1/phi)  -1      phi)

        , tetrahedron (vec3 0.118 0.691 0.191) (vec4 0 0 2 0) (vec4  0  -1      phi -(1/phi)) (vec4   1      -(1/phi) phi  0) (vec4  (1/phi)   0       phi -1     )
        , tetrahedron (vec3 0.191 0.882 0.309) (vec4 0 0 2 0) (vec4  0  -1      phi -(1/phi)) (vec4   1      -(1/phi) phi  0) (vec4   0       -1       phi (1/phi))
        , tetrahedron (vec3 0.309 0.808 0.118) (vec4 0 0 2 0) (vec4  0  -1      phi -(1/phi)) (vec4  -1      -(1/phi) phi  0) (vec4   0       -1       phi (1/phi))

        , tetrahedron (vec3 0.382 0.618 0.000) (vec4 0 0 2 0) (vec4  0  -1      phi -(1/phi)) (vec4  -1      -(1/phi) phi  0) (vec4 -(1/phi)   0       phi -1     )
        , tetrahedron (vec3 0.191 0.500 0.073) (vec4 0 0 2 0) (vec4 -1  (1/phi) phi   0     ) (vec4 -(1/phi)   0      phi  1) (vec4  -1      -(1/phi)  phi  0     )
        , tetrahedron (vec3 0.382 0.382 0.000) (vec4 0 0 2 0) (vec4 -1  (1/phi) phi   0     ) (vec4 -(1/phi)   0      phi -1) (vec4  -1      -(1/phi)  phi  0     )

        , tetrahedron (vec3 0.073 0.809 0.500) (vec4 0 0 2 0) (vec4  0  -1      phi  (1/phi)) (vec4   1      -(1/phi) phi  0) (vec4  (1/phi)   0       phi  1     )
        , tetrahedron (vec3 0.000 0.618 0.618) (vec4 0 0 2 0) (vec4  1  (1/phi) phi   0     ) (vec4  (1/phi)   0      phi  1) (vec4   1      -(1/phi)  phi  0     )
        , tetrahedron (vec3 0.000 0.618 0.382) (vec4 0 0 2 0) (vec4  1 -(1/phi) phi   0     ) (vec4  (1/phi)   0      phi -1) (vec4   1       (1/phi)  phi  0     )

        , tetrahedron (vec3 1.000 0.618 0.382) (vec4 0 2 0 0) (vec4 -(1/phi) phi  -1       0) (vec4  0 phi -(1/phi)   1     ) (vec4  -1      phi   0       (1/phi))
        , tetrahedron (vec3 0.927 0.809 0.500) (vec4 0 2 0 0) (vec4 -(1/phi) phi  -1       0) (vec4  0 phi -(1/phi)   1     ) (vec4  (1/phi) phi  -1        0     )
        , tetrahedron (vec3 1.000 0.618 0.618) (vec4 0 2 0 0) (vec4  (1/phi) phi  -1       0) (vec4  0 phi -(1/phi)   1     ) (vec4   1      phi   0       (1/phi))

        , tetrahedron (vec3 0.691 0.809 0.882) (vec4 0 2 0 0) (vec4  (1/phi) phi  -1       0) (vec4  0 phi -(1/phi)  -1     ) (vec4   1      phi   0      -(1/phi))
        , tetrahedron (vec3 0.882 0.691 0.809) (vec4 0 2 0 0) (vec4 -(1/phi) phi  -1       0) (vec4  0 phi -(1/phi)  -1     ) (vec4  -1      phi   0      -(1/phi))
        , tetrahedron (vec3 0.809 0.882 0.691) (vec4 0 2 0 0) (vec4 -(1/phi) phi  -1       0) (vec4  0 phi -(1/phi)  -1     ) (vec4  (1/phi) phi  -1        0     )

        , tetrahedron (vec3 0.618 0.618 1.000) (vec4 0 2 0 0) (vec4   0      phi  (1/phi) -1) (vec4  1 phi   0      -(1/phi)) (vec4   0      phi -(1/phi)  -1     )
        , tetrahedron (vec3 0.809 0.500 0.927) (vec4 0 2 0 0) (vec4   0      phi  (1/phi) -1) (vec4 -1 phi   0      -(1/phi)) (vec4   0      phi -(1/phi)  -1     )
        , tetrahedron (vec3 0.618 0.382 1.000) (vec4 0 2 0 0) (vec4   0      phi  (1/phi) -1) (vec4  1 phi   0      -(1/phi)) (vec4  (1/phi) phi   1        0     )

        , tetrahedron (vec3 1.000 0.382 0.382) (vec4 0 2 0 0) (vec4   0      phi -(1/phi)  1) (vec4 -1 phi   0       (1/phi)) (vec4   0      phi  (1/phi)   1     )
        , tetrahedron (vec3 1.000 0.382 0.618) (vec4 0 2 0 0) (vec4   0      phi  (1/phi)  1) (vec4  1 phi   0       (1/phi)) (vec4   0      phi -(1/phi)   1     )
        , tetrahedron (vec3 0.927 0.191 0.500) (vec4 0 2 0 0) (vec4 -(1/phi) phi   1       0) (vec4  0 phi  (1/phi)   1     ) (vec4  -1      phi   0       (1/phi))

        , tetrahedron (vec3 0.882 0.309 0.809) (vec4 0 2 0 0) (vec4   0      phi  (1/phi)  1) (vec4  1 phi   0       (1/phi)) (vec4  (1/phi) phi   1        0     )
        , tetrahedron (vec3 0.691 0.191 0.882) (vec4 0 2 0 0) (vec4 -(1/phi) phi   1       0) (vec4  0 phi  (1/phi)  -1     ) (vec4  (1/phi) phi   1        0     )
        , tetrahedron (vec3 0.809 0.118 0.691) (vec4 0 2 0 0) (vec4 -(1/phi) phi   1       0) (vec4  0 phi  (1/phi)   1     ) (vec4  (1/phi) phi   1        0     )

        , tetrahedron (vec3 0.618 1.000 0.382) (vec4 2 0 0 0) (vec4 phi -(1/phi)  0  -1     ) (vec4 phi  0   1      -(1/phi)) (vec4  phi -1       (1/phi)   0     )
        , tetrahedron (vec3 0.382 1.000 0.191) (vec4 2 0 0 0) (vec4 phi -(1/phi)  0  -1     ) (vec4 phi  0   1      -(1/phi)) (vec4  phi (1/phi)   0       -1     )
        , tetrahedron (vec3 0.500 0.927 0.382) (vec4 2 0 0 0) (vec4 phi -(1/phi)  0  -1     ) (vec4 phi  0  -1      -(1/phi)) (vec4  phi (1/phi)   0       -1     )

        , tetrahedron (vec3 0.691 0.809 0.118) (vec4 2 0 0 0) (vec4 phi -(1/phi)  0  -1     ) (vec4 phi  0  -1      -(1/phi)) (vec4  phi -1      -(1/phi)   0     )
        , tetrahedron (vec3 0.809 0.882 0.309) (vec4 2 0 0 0) (vec4 phi   0      -1  (1/phi)) (vec4 phi  1 -(1/phi)   0     ) (vec4  phi  0       -1      -(1/phi))
        , tetrahedron (vec3 0.882 0.691 0.191) (vec4 2 0 0 0) (vec4 phi   0      -1  (1/phi)) (vec4 phi -1 -(1/phi)   0     ) (vec4  phi  0       -1      -(1/phi))

        , tetrahedron (vec3 0.382 1.000 0.618) (vec4 2 0 0 0) (vec4 phi  (1/phi)  0  -1     ) (vec4 phi  0   1      -(1/phi)) (vec4  phi  1       (1/phi)   0     )
        , tetrahedron (vec3 0.500 0.927 0.809) (vec4 2 0 0 0) (vec4 phi   0       1  (1/phi)) (vec4 phi  1  (1/phi)   0     ) (vec4  phi  0        1      -(1/phi))
        , tetrahedron (vec3 0.618 1.000 0.618) (vec4 2 0 0 0) (vec4 phi   0       1 -(1/phi)) (vec4 phi -1  (1/phi)   0     ) (vec4  phi  0        1       (1/phi))

        , tetrahedron (vec3 0.809 0.118 0.309) (vec4 -1 1 1 -1) (vec4 -1 (1/phi) phi 0) (vec4 -(1/phi) phi 1 0) (vec4 -phi 1 (1/phi) 0)
        , tetrahedron (vec3 0.691 0.191 0.118) (vec4 -1 1 1 1) (vec4 -1 (1/phi) phi 0) (vec4 -(1/phi) phi 1 0) (vec4 -phi 1 (1/phi) 0)
        , tetrahedron (vec3 0.882 0.309 0.191) (vec4 1 1 -1 1) (vec4 phi 0 -1 (1/phi)) (vec4 1 0 -(1/phi) phi) (vec4 (1/phi) 0 -phi 1)

        , tetrahedron (vec3 0.382 0.382 1.000) (vec4 1 1 1 1) (vec4 1 (1/phi) phi 0) (vec4 (1/phi) phi 1 0) (vec4 phi 1 (1/phi) 0)
        , tetrahedron (vec3 0.382 0.618 1.000) (vec4 1 -1 1 1) (vec4 phi -0 1 (1/phi)) (vec4 1 -0 (1/phi) phi) (vec4 (1/phi) -0 phi 1)
        , tetrahedron (vec3 0.191 0.500 0.927) (vec4 1 1 1 -1) (vec4 1 (1/phi) phi -0) (vec4 (1/phi) phi 1 -0) (vec4 phi 1 (1/phi) -0)

        , tetrahedron (vec3 0.191 0.882 0.691) (vec4 -1 -1 1 1) (vec4 0 -phi (1/phi) 1) (vec4 0 -1 phi (1/phi)) (vec4 -0 -(1/phi) 1 phi)
        , tetrahedron (vec3 0.309 0.809 0.882) (vec4 1 -1 1 1) (vec4 0 -phi (1/phi) 1) (vec4 0 -1 phi (1/phi)) (vec4 0 -(1/phi) 1 phi)
        , tetrahedron (vec3 0.118 0.691 0.809) (vec4 1 1 1 -1) (vec4 (1/phi) 1 0 -phi) (vec4 phi (1/phi) 0 -1) (vec4 1 phi 0 -(1/phi))

        , tetrahedron (vec3 0.618 0.618 0.000) (vec4 -1 1 -1 1) (vec4 -0 phi -(1/phi) 1) (vec4 -0 1 -phi (1/phi)) (vec4 -0 (1/phi) -1 phi)
        , tetrahedron (vec3 0.618 0.382 0.000) (vec4 -1 1 1 1) (vec4 -(1/phi) 1 0 phi) (vec4 -phi (1/phi) 0 1) (vec4 -1 phi 0 (1/phi))
        , tetrahedron (vec3 0.809 0.500 0.073) (vec4 1 1 -1 1) (vec4 0 phi -(1/phi) 1) (vec4 0 1 -phi (1/phi)) (vec4 0 (1/phi) -1 phi)

        ]

--sixHundredCell : List Cell
--sixHundredCell =
--    let evenPermute : Vec4 -> List Vec4
--        evenPermute v =
--            let (a,b,c,d) = Vec4.toTuple v
--            in
--            [ vec4 a b c d
--            , vec4 a c d b
--            , vec4 a d b c
--            , vec4 b a d c
--            , vec4 b c a d
--            , vec4 b d c a
--            , vec4 c a b d
--            , vec4 c b d a
--            , vec4 c d a b
--            , vec4 d a c b
--            , vec4 d b a c
--            , vec4 d c b a
--            ]

--        plusOrMinus : Vec4 -> List Vec4
--        plusOrMinus v =
--            List.map vec4 (Vec4.getX v |> \a -> [a,-a])
--            |> andMap     (Vec4.getY v |> \a -> [a,-a])
--            |> andMap     (Vec4.getZ v |> \a -> [a,-a])
--            |> andMap     (Vec4.getW v |> \a -> [a,-a])

--        phith = 1 / phi

--        concatMap f = List.concatMap (\(a,b,c,d) -> List.map4 (,,,) (f a) (f b) (f c) (f d))
--    in
--    concatMap evenPermute -- 12 * 16 = 192
--        [ (vec4  2 0 0 0, vec4  phi  1 phith 0, vec4  phi  1 -phith 0, vec4  phi  phith 0  1)
--        , (vec4  2 0 0 0, vec4  phi  1 phith 0, vec4  phi  1 -phith 0, vec4  phi  phith 0 -1)
--        , (vec4  2 0 0 0, vec4  phi -1 phith 0, vec4  phi -1 -phith 0, vec4  phi -phith 0  1)
--        , (vec4  2 0 0 0, vec4  phi -1 phith 0, vec4  phi -1 -phith 0, vec4  phi -phith 0 -1)
--        , (vec4 -2 0 0 0, vec4 -phi  1 phith 0, vec4 -phi  1 -phith 0, vec4 -phi  phith 0  1)
--        , (vec4 -2 0 0 0, vec4 -phi  1 phith 0, vec4 -phi  1 -phith 0, vec4 -phi  phith 0 -1)
--        , (vec4 -2 0 0 0, vec4 -phi -1 phith 0, vec4 -phi -1 -phith 0, vec4 -phi -phith 0  1)
--        , (vec4 -2 0 0 0, vec4 -phi -1 phith 0, vec4 -phi -1 -phith 0, vec4 -phi -phith 0 -1)
        
--        , (vec4  1  phi 0  phith, vec4  phi  1 phith 0, vec4  phi  1 -phith 0, vec4  phi  phith 0  1)
--        , (vec4  1  phi 0 -phith, vec4  phi  1 phith 0, vec4  phi  1 -phith 0, vec4  phi  phith 0 -1)
--        , (vec4  1 -phi 0  phith, vec4  phi -1 phith 0, vec4  phi -1 -phith 0, vec4  phi -phith 0  1)
--        , (vec4  1 -phi 0 -phith, vec4  phi -1 phith 0, vec4  phi -1 -phith 0, vec4  phi -phith 0 -1)
--        , (vec4 -1  phi 0  phith, vec4 -phi  1 phith 0, vec4 -phi  1 -phith 0, vec4 -phi  phith 0  1)
--        , (vec4 -1  phi 0 -phith, vec4 -phi  1 phith 0, vec4 -phi  1 -phith 0, vec4 -phi  phith 0 -1)
--        , (vec4 -1 -phi 0  phith, vec4 -phi -1 phith 0, vec4 -phi -1 -phith 0, vec4 -phi -phith 0  1)
--        , (vec4 -1 -phi 0 -phith, vec4 -phi -1 phith 0, vec4 -phi -1 -phith 0, vec4 -phi -phith 0 -1)
--        ]
--    ++ concatMap evenPermute -- 12 * 16 = 192
--        (concatMap plusOrMinus
--            [ (vec4 phi  1 phith 0, vec4 phi  phith 0  1, vec4 1 1 1 1, vec4 1 phi 0 phith)
--            ])
--    ++ concatMap plusOrMinus --16 * 12 = 192
--        [ (vec4 2 0 0 0, vec4 phi 1 phith 0, vec4 phi phith 0 1, vec4 phi 0 1 phith)
--        , (vec4 1 1 1 1, vec4 phi 1 phith 0, vec4 phi phith 0 1, vec4 phi 0 1 phith)
--        , (vec4 0 2 0 0, vec4 1 phi 0 phith, vec4 phith phi 1 0, vec4 0 phi phith 1)
--        , (vec4 1 1 1 1, vec4 1 phi 0 phith, vec4 phith phi 1 0, vec4 0 phi phith 1)
--        , (vec4 0 0 2 0, vec4 phith 0 phi 1, vec4 0 1 phi phith, vec4 1 phith phi 0)
--        , (vec4 1 1 1 1, vec4 phith 0 phi 1, vec4 0 1 phi phith, vec4 1 phith phi 0)
--        , (vec4 0 0 0 2, vec4 0 phith 1 phi, vec4 1 0 phith phi, vec4 phith 1 0 phi)
--        , (vec4 1 1 1 1, vec4 0 phith 1 phi, vec4 1 0 phith phi, vec4 phith 1 0 phi)
        
--        , (vec4 1 1 1 1, vec4 0 phi phith 1, vec4 0 1 phi phith, vec4 0 phith 1 phi)
--        , (vec4 1 1 1 1, vec4 phi 0 1 phith, vec4 1 0 phith phi, vec4 phith 0 phi 1)
--        , (vec4 1 1 1 1, vec4 phith 1 0 phi, vec4 phi phith 0 1, vec4 1 phi 0 phith)
--        , (vec4 1 1 1 1, vec4 1 phith phi 0, vec4 phith phi 1 0, vec4 phi 1 phith 0)
--        ]
--    ++ -- 24
--        [ (vec4  phi  1 phith 0, vec4  phi  1 -phith 0, vec4  1  phi 0 phith, vec4  1  phi 0 -phith)
--        , (vec4  phi -1 phith 0, vec4  phi -1 -phith 0, vec4  1 -phi 0 phith, vec4  1 -phi 0 -phith)
--        , (vec4 -phi  1 phith 0, vec4 -phi  1 -phith 0, vec4 -1  phi 0 phith, vec4 -1  phi 0 -phith)
--        , (vec4 -phi -1 phith 0, vec4 -phi -1 -phith 0, vec4 -1 -phi 0 phith, vec4 -1 -phi 0 -phith)

--        , (vec4  phi 0  1 phith, vec4  phi 0  1 -phith, vec4  1 phith  phi 0, vec4  1 -phith  phi 0)
--        , (vec4  phi 0 -1 phith, vec4  phi 0 -1 -phith, vec4  1 phith -phi 0, vec4  1 -phith -phi 0)
--        , (vec4 -phi 0  1 phith, vec4 -phi 0  1 -phith, vec4 -1 phith  phi 0, vec4 -1 -phith  phi 0)
--        , (vec4 -phi 0 -1 phith, vec4 -phi 0 -1 -phith, vec4 -1 phith -phi 0, vec4 -1 -phith -phi 0)
        
--        , (vec4  phi phith 0  1, vec4  phi -phith 0  1, vec4  1 0 phith  phi, vec4  1 0 -phith  phi)
--        , (vec4  phi phith 0 -1, vec4  phi -phith 0 -1, vec4  1 0 phith -phi, vec4  1 0 -phith -phi)
--        , (vec4 -phi phith 0  1, vec4 -phi -phith 0  1, vec4 -1 0 phith  phi, vec4 -1 0 -phith  phi)
--        , (vec4 -phi phith 0 -1, vec4 -phi -phith 0 -1, vec4 -1 0 phith -phi, vec4 -1 0 -phith -phi)
        
--        , (vec4 phith 0  phi  1, vec4 -phith 0  phi  1, vec4 0 phith  1  phi, vec4 0 -phith  1  phi)
--        , (vec4 phith 0  phi -1, vec4 -phith 0  phi -1, vec4 0 phith  1 -phi, vec4 0 -phith  1 -phi)
--        , (vec4 phith 0 -phi  1, vec4 -phith 0 -phi  1, vec4 0 phith -1  phi, vec4 0 -phith -1  phi)
--        , (vec4 phith 0 -phi -1, vec4 -phith 0 -phi -1, vec4 0 phith -1 -phi, vec4 0 -phith -1 -phi)

--        , (vec4 0  phi phith  1, vec4 0  phi -phith  1, vec4 phith  1 0  phi, vec4 -phith  1 0  phi)
--        , (vec4 0  phi phith -1, vec4 0  phi -phith -1, vec4 phith  1 0 -phi, vec4 -phith  1 0 -phi)
--        , (vec4 0 -phi phith  1, vec4 0 -phi -phith  1, vec4 phith -1 0  phi, vec4 -phith -1 0  phi)
--        , (vec4 0 -phi phith -1, vec4 0 -phi -phith -1, vec4 phith -1 0 -phi, vec4 -phith -1 0 -phi)
        
--        , (vec4 phith  phi  1 0, vec4 -phith  phi  1 0, vec4 0  1  phi phith, vec4 0  1  phi -phith)
--        , (vec4 phith  phi -1 0, vec4 -phith  phi -1 0, vec4 0  1 -phi phith, vec4 0  1 -phi -phith)
--        , (vec4 phith -phi  1 0, vec4 -phith -phi  1 0, vec4 0 -1  phi phith, vec4 0 -1  phi -phith)
--        , (vec4 phith -phi -1 0, vec4 -phith -phi -1 0, vec4 0 -1 -phi phith, vec4 0 -1 -phi -phith)
--        ]
--    |> List.map 
--        (\(a,b,c,d) -> 
--            tetrahedron 
--                (avg [a,b,c,d] 
--                    |> Vec4.toTuple
--                    |> mapTuple4 (\x -> (x+1)/2)
--                    |> \(a,b,c,d) -> vec3 (4*a*c+4*b*d) (4*b*c-4*a*d) (a^2+b^2-c^2-d^2)) 
--                a b c d)
















