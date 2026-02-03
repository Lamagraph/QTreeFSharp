module Matrix

open Common
(*
| x1 | x2 |
----------
| x3 | x4 |
*)
type qtree<'value> = //when 'TCell: equality> =
    | Node of qtree<'value> * qtree<'value> * qtree<'value> * qtree<'value>
    | Leaf of 'value


[<Measure>]
type columnId

[<Measure>]
type rowId


[<Struct>]
type SparseMatrix<'value> =

    val nrows: uint<rowId>

    val ncols: uint<columnId>

    val nvals: uint<nvals>

    val storage: qtree<Option<'value>>

    new(_nrows, _ncols, _nvals, _storage) =
        { nrows = _nrows
          ncols = _ncols
          nvals = _nvals
          storage = _storage }


let fromCoo size (coo: List<uint<columnId> * uint<rowId> * 'value>) =
    let groupedByColumns =
        List.groupBy (fun (i, j, v) -> i) coo
        |> List.sortBy fst
        |> List.map (fun (x, l) -> x, List.sortBy (fun (_, j, _) -> j) l)

    0

let mkNode x1 x2 x3 x4 =
    match (x1, x2, x3, x4) with
    | Leaf(v1), Leaf(v2), Leaf(v3), Leaf(v4) when v1 = v2 && v2 = v3 && v3 = v4 -> Leaf(v1)
    | _ -> Node(x1, x2, x3, x4)

(*
let rec map (op: 'TCell1 -> 'TCell2)  (m1:Matrix<'TCell1>) =
    match m1 with
    | Leaf(s,v1) ->
        Leaf (s, op v1)
    | Node (s,x1,x2,x3,x4) ->
        mkNode s (map op x1) (map op x2) (map op x3) (map op x4)

let map2 (op: 'TCell1 -> 'TCell2 -> 'TCell3)  (m1:Matrix<'TCell1>) (m2:Matrix<'TCell2>) =
    let rec go m1 m2 =
        match (m1, m2) with
        | Leaf(s,v1), Leaf(_,v2) ->
            Leaf (s, op v1 v2)
        | Node (s,x1,x2,x3,x4), Node (_,y1,y2,y3,y4) ->
            mkNode s (go x1 y1) (go x2 y2) (go x3 y3) (go x4 y4)                
        | Node (s,x1,x2,x3,x4), Leaf (_,v) ->
            let l = Leaf(s / 2u,v)
            mkNode s (go x1 l) (go x2 l) (go x3 l) (go x4 l)                
        | Leaf (_,v), Node (s,x1,x2,x3,x4) ->
            let l = Leaf(s / 2u,v)
            mkNode s (go l x1) (go l x2) (go l x3) (go l x4)
            
    if m1.Size = m2.Size
    then go m1 m2
    else failwith $"Matrices should be of equals size, but m1.Size = {m1.Size} and m2.Size = {m2.Size}"

let multScalar opAdd (x:uint) y =
    if x = 1u
    then y
    else
        List.init (int x) (fun _ -> y)
        |> List.reduce opAdd

let compose opAdd (opMult: 'TCell1 -> 'TCell2 -> 'TCell3) (m1:Matrix<'TCell1>) (m2:Matrix<'TCell2>) =
    
    let rec go m1 m2 =
        match (m1, m2) with
        | Leaf(s,v1), Leaf(_,v2) ->
            Leaf (s, multScalar opAdd s (opMult v1 v2))
        | Node (s,x1,x2,x3,x4), Node (_,y1,y2,y3,y4) ->
            let z1 = map2 opAdd (go x1 y1) (go x2 y3)
            let z2 = map2 opAdd (go x1 y2) (go x2 y4)
            let z3 = map2 opAdd (go x3 y1) (go x4 y3)
            let z4 = map2 opAdd (go x3 y2) (go x4 y4)
            mkNode s z1 z2 z3 z4
        | Node (s,x1,x2,x3,x4), Leaf (_,v) ->
            let l = Leaf(s / 2u,v)                
            let z1 = map2 opAdd (go x1 l) (go x2 l)                
            let z3 = map2 opAdd (go x3 l) (go x4 l)                
            mkNode s z1 z1 z3 z3                
        | Leaf (_,v), Node (s,x1,x2,x3,x4) ->
            let l = Leaf(s / 2u,v)
            let z1 = map2 opAdd (go l x1) (go l x3)
            let z2 = map2 opAdd (go l x2) (go l x4)                
            mkNode s z1 z2 z1 z2
            
    if m1.Size = m2.Size
    then go m1 m2
    else failwith $"Matrices should be of equals size, but m1.Size = {m1.Size} and m2.Size = {m2.Size}"

*)

(*
module UserCode =
open MyLibrary

type CellOfMyMatrix<'TVal> =
    | EmptyCell
    | FilledCell of 'TVal
    
let veryUsefulFunction m1 m2 m3 =
    let op1 x y =
        match (x,y) with
        | EmptyCell, EmptyCell -> true
        | _ -> false
    let op2 x y =
        match (x,y) with
        | true, FilledCell x -> FilledCell (x + 1)
        | false, FilledCell x ->
            let res = x - 1
            if res = 0 then EmptyCell else FilledCell res
        | _ -> EmptyCell
    map2 op2 (map2 op1 m1 m2) m3
    
let veryUsefulFunction2 m1 m2 m3 =
    let op1 = (+)
    let op2  = (*)
    map2 op2 (map2 op1 m1 m2) m3*)
