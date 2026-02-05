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

[<Struct>]
type Storage<'value> =
    // Storage is always size-x-size square.
    val size: uint64<storageSize>
    val data: qtree<'value>

    new(_size, _data) = { size = _size; data = _data }

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

[<Measure>]
type rowindex

[<Measure>]
type colindex

type COOEntry<'value> = uint64<rowindex> * uint64<colindex> * 'value

[<Struct>]
type CoordinateList<'value> =
    val nrows: uint64<nrows>
    val ncols: uint64<ncols>
    val list: COOEntry<'value> list

    new(_nrows, _ncols, _list) =
        { nrows = _nrows
          ncols = _ncols
          list = _list }

let private getQuadrantCoords (pr, pc) halfSize =
    (pr, pc), // NORTH WEST
    (pr, pc + halfSize * 1UL<colindex>), // NORTH EAST
    (pr + halfSize * 1UL<rowindex>, pc), // SOUTH WEST
    (pr + halfSize * 1UL<rowindex>, pc + halfSize * 1UL<colindex>) // SOUTH EAST

let fromCoordinateList (coo: CoordinateList<'a>) =
    let nvals = (uint64 <| List.length coo.list) * 1UL<nvals>
    let nrows = coo.nrows
    let ncols = coo.ncols

    // the resulting matrix is always square
    let storageSize = getNearestUpperPowerOfTwo (max (uint64 nrows) (uint64 ncols))

    let isEntryInQuadrant (pr, pc) size (entry: COOEntry<'a>) =
        let (i, j, _) = entry

        i >= pr
        && j >= pc
        && i < pr + size * 1UL<rowindex>
        && j < pc + size * 1UL<colindex>

    let rec traverse coordinates (pr, pc) size =
        match coordinates with
        | [] when (uint64 pr) + size < uint64 nrows && (uint64 pc) + size < uint64 ncols -> Leaf <| UserValue None
        | [] when uint64 pr >= uint64 nrows || uint64 pc >= uint64 ncols -> Leaf Dummy
        | (i, j, value) :: _ when pr = i && pc = j && size = 1UL -> Leaf << UserValue <| Some value
        | _ ->
            let halfSize = size / 2UL
            let nwp, nep, swp, sep = getQuadrantCoords (pr, pc) halfSize
            let nwCoo = coordinates |> List.filter (isEntryInQuadrant nwp halfSize)
            let neCoo = coordinates |> List.filter (isEntryInQuadrant nep halfSize)
            let swCoo = coordinates |> List.filter (isEntryInQuadrant swp halfSize)
            let seCoo = coordinates |> List.filter (isEntryInQuadrant sep halfSize)

            mkNode
                (traverse nwCoo nwp halfSize)
                (traverse neCoo nep halfSize)
                (traverse swCoo swp halfSize)
                (traverse seCoo sep halfSize)

    let tree = traverse coo.list (0UL<rowindex>, 0UL<colindex>) storageSize

    SparseMatrix(nrows, ncols, nvals, Storage(storageSize * 1UL<storageSize>, tree))

let toCoordinateList (matrix: SparseMatrix<'a>) =
    let nrows = matrix.nrows
    let ncols = matrix.ncols

    let rec traverse tree (pr, pc) size =
        match tree with
        | Leaf Dummy
        | Leaf(UserValue None) -> []
        | Leaf(UserValue(Some value)) ->
            [ for i in uint64 pr .. (uint64 pr) + size - 1UL do
                  for j in uint64 pc .. (uint64 pc) + size - 1UL -> (i * 1UL<rowindex>, j * 1UL<colindex>, value) ]
        | Node(nw, ne, sw, se) ->
            let halfSize = size / 2UL
            let nwp, nep, swp, sep = getQuadrantCoords (pr, pc) halfSize

            traverse nw nwp halfSize
            @ traverse ne nep halfSize
            @ traverse sw swp halfSize
            @ traverse se sep halfSize

    let coo =
        traverse matrix.storage.data (0UL<rowindex>, 0UL<colindex>) (uint64 matrix.storage.size)

    CoordinateList(nrows, ncols, coo)

let map2 (matrix1: SparseMatrix<_>) (matrix2: SparseMatrix<_>) f =
    let rec inner (size: uint64<storageSize>) matrix1 matrix2 =
        let _do x1 x2 x3 x4 y1 y2 y3 y4 =
            let new_size = size / 2UL

            match (inner new_size x1 y1), (inner new_size x2 y2), (inner new_size x3 y3), (inner new_size x4 y4) with
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
                | _ -> (uint64 size) * (uint64 size) * 1UL<nvals>

            (Leaf(UserValue(res)), nnz) |> Result.Success

        | Node(x1, x2, x3, x4), Node(y1, y2, y3, y4) -> _do x1 x2 x3 x4 y1 y2 y3 y4
        | Node(x1, x2, x3, x4), Leaf(v) -> _do x1 x2 x3 x4 matrix2 matrix2 matrix2 matrix2
        | Leaf(v), Node(x1, x2, x3, x4) -> _do matrix1 matrix1 matrix1 matrix1 x1 x2 x3 x4
        | (x, y) -> Result.Failure <| Error.InconsistentStructureOfStorages(x, y)

    if matrix1.nrows = matrix2.nrows && matrix1.ncols = matrix2.ncols then
        match inner matrix1.storage.size matrix1.storage.data matrix2.storage.data with
        | Result.Failure x -> Result.Failure x
        | Result.Success(storage, nvals) ->
            (SparseMatrix(matrix1.nrows, matrix1.ncols, nvals, (Storage(matrix1.storage.size, storage))))
            |> Result.Success
    else
        (Error.InconsistentSizeOfArguments(matrix1, matrix2)) |> Result.Failure

let fold (folder: 'State option -> 'T option -> 'State option) (state: 'State option) (matrix: SparseMatrix<'T>) =
    let rec traverse tree (size: uint64<storageSize>) (state: 'State option) =
        match tree with
        | Leaf Dummy -> state
        | Leaf(UserValue v) ->
            let area = (uint64 size) * (uint64 size)

            let rec foldArea count accum =
                if count = 0UL then
                    accum
                else
                    foldArea (count - 1UL) (folder accum v)

            foldArea area state
        | Node(nw, ne, sw, se) ->
            let halfSize = size / 2UL

            let nwState = traverse nw halfSize state
            let neState = traverse ne halfSize nwState
            let swState = traverse sw halfSize neState
            let seState = traverse se halfSize swState

            seState

    let storageSize = matrix.storage.size

    let tree = matrix.storage.data
    traverse tree storageSize state


let getLowerTriangle (matrix: SparseMatrix<_>) =

    // returns tree, removed_nvals
    let rec makeNone tree (size: uint64<storageSize>) =
        match tree with
        | Leaf Dummy
        | Leaf(UserValue None) -> tree, 0UL<nvals>
        | Leaf(UserValue(Some _)) -> Leaf(UserValue None), (uint64 <| size * size) * 1UL<nvals>
        | Node(nw, ne, sw, se) ->
            let halfSize = size / 2UL
            let nw_new, nw_removed = makeNone nw halfSize
            let ne_new, ne_removed = makeNone ne halfSize
            let sw_new, sw_removed = makeNone sw halfSize
            let se_new, se_removed = makeNone se halfSize

            (mkNode nw_new ne_new sw_new se_new), nw_removed + ne_removed + sw_removed + se_removed

    let rec traverse tree size =
        match tree with
        | Leaf _ when size = 1UL<storageSize> -> tree, 0UL<nvals>
        | Leaf Dummy -> Leaf Dummy, 0UL<nvals>
        | Leaf _ ->
            let halfSize = size / 2UL

            let nw, nw_removed = traverse tree halfSize

            let ne, ne_removed =
                Leaf <| UserValue None, (uint64 <| halfSize * halfSize) * 1UL<nvals>

            let sw, sw_removed = tree, 0UL<nvals>
            let se, se_removed = traverse tree halfSize
            (mkNode nw ne sw se), nw_removed + ne_removed + sw_removed + se_removed
        | Node(nw, ne, sw, se) ->
            let halfSize = size / 2UL

            let nw_new, nw_removed = traverse nw halfSize
            let ne_new, ne_removed = makeNone ne halfSize
            let sw_new, sw_removed = sw, 0UL<nvals>
            let se_new, se_removed = traverse se halfSize

            (mkNode nw_new ne_new sw_new se_new), nw_removed + ne_removed + sw_removed + se_removed

    let storageSize = matrix.storage.size
    let tree, nvals_removed = traverse matrix.storage.data storageSize

    SparseMatrix(matrix.nrows, matrix.ncols, matrix.nvals - nvals_removed, Storage(storageSize, tree))

let transpose (matrix: SparseMatrix<_>) =
    let rec traverse tree =
        match tree with
        | Leaf _ -> tree
        | Node(nw, ne, sw, se) ->
            mkNode
                (traverse nw)
                (traverse sw) // ne -> sw
                (traverse ne) // sw -> ne
                (traverse se)

    let nrows = (uint64 matrix.ncols) * 1UL<nrows>
    let ncols = (uint64 matrix.nrows) * 1UL<ncols>

    let tree = traverse matrix.storage.data

    SparseMatrix(nrows, ncols, matrix.nvals, Storage(matrix.storage.size, tree))

let mask (m1: SparseMatrix<'a>) (m2: SparseMatrix<'b>) f =
    map2 m1 m2 (fun m1 m2 -> if f m2 then m1 else None)
