module Matrix

open Common

(*
| x1 | x2 |
----------
| x3 | x4 |
*)
type qtree<'value> =
    | Node of qtree<'value> * qtree<'value> * qtree<'value> * qtree<'value>
    | Leaf of treeValue<'value>

[<Measure>]
type ncols

[<Measure>]
type nrows

[<Measure>]
type storageVSize

[<Measure>]
type storageHSize

[<Struct>]
type Storage<'value> =
    val vSize: uint64<storageVSize>
    val hSize: uint64<storageHSize>
    val data: qtree<'value>

    new(_vSize, _hSize, _data) =
        { vSize = _vSize
          hSize = _hSize
          data = _data }

[<Struct>]
type SparseMatrix<'value> =
    val nrows: uint64<nrows>
    val ncols: uint64<ncols>
    val nvals: uint64<nvals>
    val storage: Storage<Option<'value>>

    new(_nrows, _ncols, _nvals, _storage) =
        { nrows = _nrows
          ncols = _ncols
          nvals = _nvals
          storage = _storage }

type Error<'value1, 'value2> =
    | InconsistentStructureOfStorages of qtree<Option<'value1>> * qtree<Option<'value2>>
    | InconsistentSizeOfArguments of SparseMatrix<'value1> * SparseMatrix<'value2>


let mkNode x1 x2 x3 x4 =
    match (x1, x2, x3, x4) with
    | Leaf(v1), Leaf(v2), Leaf(v3), Leaf(v4) when v1 = v2 && v2 = v3 && v3 = v4 -> Leaf(v1)
    | _ -> Node(x1, x2, x3, x4)

type COOEntry<'value> = uint64 * uint64 * 'value

[<Struct>]
type CoordinateList<'value> =
    val nrows: uint64<nrows>
    val ncols: uint64<ncols>
    val list: COOEntry<'value> list

    new(_nrows, _ncols, _list) =
        { nrows = _nrows
          ncols = _ncols
          list = _list }

let private getQuadrantCoords (px, py) halfSize =
    (px, py), (px + halfSize, py), (px, py + halfSize), (px + halfSize, py + halfSize)

let fromCoordinateList (coo: CoordinateList<'a>) =
    let nvals = (uint64 <| List.length coo.list) * 1UL<nvals>
    let nrows = coo.nrows
    let ncols = coo.ncols

    // the resulting matrix is always square
    let storageSize = getNearestUpperPowerOfTwo (max (uint64 nrows) (uint64 ncols))

    let isEntryInQuadrant (px, py) size (entry: COOEntry<'a>) =
        let (i, j, _) = entry
        i >= px && j >= py && i < px + size && j < py + size

    let rec traverse coordinates (px, py) size =
        match coordinates with
        | [] when px + size < uint64 nrows && py + size < uint64 ncols -> Leaf <| UserValue None
        | [] when px >= uint64 nrows || py >= uint64 ncols -> Leaf Dummy
        | (i, j, value) :: _ when px = i && py = j && size = 1UL -> Leaf << UserValue <| Some value
        | _ ->
            let halfSize = size / 2UL
            let nwp, nep, swp, sep = getQuadrantCoords (px, py) halfSize
            let nwCoo = coordinates |> List.filter (isEntryInQuadrant nwp halfSize)
            let neCoo = coordinates |> List.filter (isEntryInQuadrant nep halfSize)
            let swCoo = coordinates |> List.filter (isEntryInQuadrant swp halfSize)
            let seCoo = coordinates |> List.filter (isEntryInQuadrant sep halfSize)

            mkNode
                (traverse nwCoo nwp halfSize)
                (traverse neCoo nep halfSize)
                (traverse swCoo swp halfSize)
                (traverse seCoo sep halfSize)

    let tree = traverse coo.list (0UL, 0UL) storageSize

    SparseMatrix(nrows, ncols, nvals, Storage(storageSize * 1UL<storageVSize>, storageSize * 1UL<storageHSize>, tree))

let toCoordinateList (matrix: SparseMatrix<'a>) =
    let nrows = matrix.nrows
    let ncols = matrix.ncols

    let rec traverse tree (px, py) size =
        match tree with
        | Leaf Dummy
        | Leaf(UserValue None) -> []
        | Leaf(UserValue(Some value)) ->
            [ for i in px .. px + size - 1UL do
                  for j in py .. py + size - 1UL -> (i, j, value) ]
        | Node(nw, ne, sw, se) ->
            let halfSize = size / 2UL
            let nwp, nep, swp, sep = getQuadrantCoords (px, py) halfSize

            traverse nw nwp halfSize
            @ traverse ne nep halfSize
            @ traverse sw swp halfSize
            @ traverse se sep halfSize

    let coo =
        traverse matrix.storage.data (0UL, 0UL) (max (uint64 matrix.storage.hSize) (uint64 matrix.storage.vSize))

    CoordinateList(nrows, ncols, coo)

let map2 (matrix1: SparseMatrix<_>) (matrix2: SparseMatrix<_>) f =
    let rec inner (vSize: uint64<storageVSize>) (hSize: uint64<storageHSize>) matrix1 matrix2 =
        let _do x1 x2 x3 x4 y1 y2 y3 y4 =
            let new_vSize = vSize / 2UL
            let new_hSize = hSize / 2UL

            match
                (inner new_vSize new_hSize x1 y1),
                (inner new_vSize new_hSize x2 y2),
                (inner new_vSize new_hSize x3 y3),
                (inner new_vSize new_hSize x4 y4)
            with
            | Result.Success((new_t1, nvals1)),
              Result.Success((new_t2, nvals2)),
              Result.Success((new_t3, nvals3)),
              Result.Success((new_t4, nvals4)) ->
                ((mkNode new_t1 new_t2 new_t3 new_t4), nvals1 + nvals2 + nvals3 + nvals4)
                |> Result.Success
            | Result.Failure(e), _, _, _
            | _, Result.Failure(e), _, _
            | _, _, Result.Failure(e), _
            | _, _, _, Result.Failure(e) -> Result.Failure(e)

        match (matrix1, matrix2) with
        | Leaf(Dummy), Leaf(Dummy) -> Result.Success(Leaf(Dummy), 0UL<nvals>)
        | Leaf(UserValue(v1)), Leaf(UserValue(v2)) ->
            let res = f v1 v2

            let nnz =
                match res with
                | None -> 0UL<nvals>
                | _ -> (uint64 vSize) * (uint64 hSize) * 1UL<nvals>

            (Leaf(UserValue(res)), nnz) |> Result.Success

        | Node(x1, x2, x3, x4), Node(y1, y2, y3, y4) -> _do x1 x2 x3 x4 y1 y2 y3 y4
        | Node(x1, x2, x3, x4), Leaf(v) -> _do x1 x2 x3 x4 matrix2 matrix2 matrix2 matrix2
        | Leaf(v), Node(x1, x2, x3, x4) -> _do matrix1 matrix1 matrix1 matrix1 x1 x2 x3 x4
        | (x, y) -> Result.Failure <| Error.InconsistentStructureOfStorages(x, y)

    if matrix1.nrows = matrix2.nrows && matrix1.ncols = matrix2.ncols then
        match inner matrix1.storage.vSize matrix1.storage.hSize matrix1.storage.data matrix2.storage.data with
        | Result.Failure x -> Result.Failure x
        | Result.Success(storage, nvals) ->
            (SparseMatrix(
                matrix1.nrows,
                matrix1.ncols,
                nvals,
                (Storage(matrix1.storage.vSize, matrix1.storage.hSize, storage))
            ))
            |> Result.Success
    else
        (Error.InconsistentSizeOfArguments(matrix1, matrix2)) |> Result.Failure


(*
let rec map (op: 'TCell1 -> 'TCell2)  (m1:Matrix<'TCell1>) =
    match m1 with
    | Leaf(s,v1) ->
        Leaf (s, op v1)
    | Node (s,x1,x2,x3,x4) ->
        mkNode s (map op x1) (map op x2) (map op x3) (map op x4)


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
