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

type Error =
    | InconsistentStructureOfStorages
    | InconsistentSizeOfArguments


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
    let unique =
        coo.list
        |> List.groupBy (fun (i, j, _) -> (i, j))
        |> List.map (fun ((i, j), entries) ->
            let value = entries |> List.map (fun (_, _, v) -> v) |> List.last
            (i, j, value))

    if
        unique
        |> List.exists (fun (i, j, _) -> uint64 i >= uint64 coo.nrows || uint64 j >= uint64 coo.ncols)
    then
        failwith "Coordinates out of range"

    let nvals = (uint64 <| List.length unique) * 1UL<nvals>
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

    let tree = traverse unique (0UL<rowindex>, 0UL<colindex>) storageSize

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

    let sorted = List.sort coo
    CoordinateList(nrows, ncols, sorted)

let empty nrows ncols =
    fromCoordinateList (CoordinateList(nrows, ncols, []))

let map (matrix: SparseMatrix<'a>) f =
    let rec inner (size: uint64<storageSize>) (tree: qtree<Option<'a>>) =
        match tree with
        | Node(nw, ne, sw, se) ->
            let nwTree, nwNvals = inner (size / 2UL) nw
            let neTree, neNvals = inner (size / 2UL) ne
            let swTree, swNvals = inner (size / 2UL) sw
            let seTree, seNvals = inner (size / 2UL) se
            (mkNode nwTree neTree swTree seTree), nwNvals + neNvals + swNvals + seNvals
        | Leaf(Dummy) -> Leaf(Dummy), 0UL<nvals>
        | Leaf(UserValue(v)) ->
            let res = f v

            let nnz =
                match res with
                | None -> 0UL<nvals>
                | _ -> (uint64 size) * (uint64 size) * 1UL<nvals>

            Leaf(UserValue(res)), nnz

    let newTree, newNvals = inner matrix.storage.size matrix.storage.data
    SparseMatrix(matrix.nrows, matrix.ncols, newNvals, Storage(matrix.storage.size, newTree))

let map2 (matrix1: SparseMatrix<_>) (matrix2: SparseMatrix<_>) f =
    let rec inner (size: uint64<storageSize>) matrix1 matrix2 =
        let _do x1 x2 x3 x4 y1 y2 y3 y4 =
            let new_size = size / 2UL

            match (inner new_size x1 y1), (inner new_size x2 y2), (inner new_size x3 y3), (inner new_size x4 y4) with
            | Ok((new_t1, nvals1)), Ok((new_t2, nvals2)), Ok((new_t3, nvals3)), Ok((new_t4, nvals4)) ->
                ((mkNode new_t1 new_t2 new_t3 new_t4), nvals1 + nvals2 + nvals3 + nvals4) |> Ok
            | Error(e), _, _, _
            | _, Error(e), _, _
            | _, _, Error(e), _
            | _, _, _, Error(e) -> Error(e)

        match (matrix1, matrix2) with
        | Leaf(Dummy), Leaf(Dummy) -> Ok(Leaf(Dummy), 0UL<nvals>)
        | Leaf(UserValue(v1)), Leaf(UserValue(v2)) ->
            let res = f v1 v2

            let nnz =
                match res with
                | None -> 0UL<nvals>
                | _ -> (uint64 size) * (uint64 size) * 1UL<nvals>

            (Leaf(UserValue(res)), nnz) |> Ok

        | Node(x1, x2, x3, x4), Node(y1, y2, y3, y4) -> _do x1 x2 x3 x4 y1 y2 y3 y4
        | Node(x1, x2, x3, x4), Leaf(v) -> _do x1 x2 x3 x4 matrix2 matrix2 matrix2 matrix2
        | Leaf(v), Node(x1, x2, x3, x4) -> _do matrix1 matrix1 matrix1 matrix1 x1 x2 x3 x4
        | (x, y) -> Error Error.InconsistentStructureOfStorages

    if matrix1.nrows = matrix2.nrows && matrix1.ncols = matrix2.ncols then
        match inner matrix1.storage.size matrix1.storage.data matrix2.storage.data with
        | Error x -> Error x
        | Ok(storage, nvals) ->
            (SparseMatrix(matrix1.nrows, matrix1.ncols, nvals, (Storage(matrix1.storage.size, storage))))
            |> Ok
    else
        Error Error.InconsistentSizeOfArguments

let map2i (matrix1: SparseMatrix<_>) (matrix2: SparseMatrix<_>) f =
    let rec inner (prow: uint64<rowindex>) (pcol: uint64<colindex>) (size: uint64<storageSize>) matrix1 matrix2 =
        match (matrix1, matrix2) with
        | Node(x1, x2, x3, x4), Node(y1, y2, y3, y4) ->
            let halfSize = size / 2UL

            let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                getQuadrantCoords (prow, pcol) (uint64 halfSize)

            let t1, nvals1 = inner nwR nwC halfSize x1 y1
            let t2, nvals2 = inner neR neC halfSize x2 y2
            let t3, nvals3 = inner swR swC halfSize x3 y3
            let t4, nvals4 = inner seR seC halfSize x4 y4
            (mkNode t1 t2 t3 t4), nvals1 + nvals2 + nvals3 + nvals4
        | Node(x1, x2, x3, x4), Leaf(v2) ->
            let halfSize = size / 2UL

            let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                getQuadrantCoords (prow, pcol) (uint64 halfSize)

            let t1, nvals1 = inner nwR nwC halfSize x1 (Leaf(v2))
            let t2, nvals2 = inner neR neC halfSize x2 (Leaf(v2))
            let t3, nvals3 = inner swR swC halfSize x3 (Leaf(v2))
            let t4, nvals4 = inner seR seC halfSize x4 (Leaf(v2))
            (mkNode t1 t2 t3 t4), nvals1 + nvals2 + nvals3 + nvals4
        | Leaf(v1), Node(y1, y2, y3, y4) ->
            let halfSize = size / 2UL

            let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                getQuadrantCoords (prow, pcol) (uint64 halfSize)

            let t1, nvals1 = inner nwR nwC halfSize (Leaf(v1)) y1
            let t2, nvals2 = inner neR neC halfSize (Leaf(v1)) y2
            let t3, nvals3 = inner swR swC halfSize (Leaf(v1)) y3
            let t4, nvals4 = inner seR seC halfSize (Leaf(v1)) y4
            (mkNode t1 t2 t3 t4), nvals1 + nvals2 + nvals3 + nvals4
        | Leaf(Dummy), Leaf(Dummy) -> Leaf(Dummy), 0UL<nvals>
        | Leaf(UserValue(v1)), Leaf(UserValue(v2)) ->
            let res = f prow pcol v1 v2

            let nnz =
                match res with
                | Some _ -> 1UL<nvals>
                | None -> 0UL<nvals>

            Leaf(UserValue(res)), nnz
        | Leaf(UserValue(v)), Leaf(Dummy) ->
            let res = f prow pcol v None

            let nnz =
                match res with
                | Some _ -> 1UL<nvals>
                | None -> 0UL<nvals>

            Leaf(UserValue(res)), nnz
        | Leaf(Dummy), Leaf(UserValue(v)) ->
            let res = f prow pcol None v

            let nnz =
                match res with
                | Some _ -> 1UL<nvals>
                | None -> 0UL<nvals>

            Leaf(UserValue(res)), nnz

    if matrix1.nrows = matrix2.nrows && matrix1.ncols = matrix2.ncols then
        let storage, nvals =
            inner 0UL<rowindex> 0UL<colindex> matrix1.storage.size matrix1.storage.data matrix2.storage.data

        SparseMatrix(matrix1.nrows, matrix1.ncols, nvals, (Storage(matrix1.storage.size, storage)))
        |> Ok
    else
        Error Error.InconsistentSizeOfArguments

let mapi (matrix: SparseMatrix<'a>) f =
    let rec inner (prow: uint64<rowindex>) (pcol: uint64<colindex>) (size: uint64<storageSize>) matrix =
        match matrix with
        | Node(x1, x2, x3, x4) ->
            let halfSize = size / 2UL

            let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                getQuadrantCoords (prow, pcol) (uint64 halfSize)

            let t1, nvals1 = inner nwR nwC halfSize x1
            let t2, nvals2 = inner neR neC halfSize x2
            let t3, nvals3 = inner swR swC halfSize x3
            let t4, nvals4 = inner seR seC halfSize x4
            (mkNode t1 t2 t3 t4), nvals1 + nvals2 + nvals3 + nvals4
        | Leaf(Dummy) -> Leaf(Dummy), 0UL<nvals>
        | Leaf(UserValue(v)) ->
            if size = 1UL<storageSize> then
                let res = f prow pcol v

                let nnz =
                    match res with
                    | Some _ -> 1UL<nvals>
                    | None -> 0UL<nvals>

                Leaf(UserValue(res)), nnz
            else
                let halfSize = size / 2UL

                let (nwR, nwC), (neR, neC), (swR, swC), (seR, seC) =
                    getQuadrantCoords (prow, pcol) (uint64 halfSize)

                let t1, nvals1 = inner nwR nwC halfSize (Leaf(UserValue(v)))
                let t2, nvals2 = inner neR neC halfSize (Leaf(UserValue(v)))
                let t3, nvals3 = inner swR swC halfSize (Leaf(UserValue(v)))
                let t4, nvals4 = inner seR seC halfSize (Leaf(UserValue(v)))
                (mkNode t1 t2 t3 t4), nvals1 + nvals2 + nvals3 + nvals4

    let storage, nvals =
        inner 0UL<rowindex> 0UL<colindex> matrix.storage.size matrix.storage.data

    SparseMatrix(matrix.nrows, matrix.ncols, nvals, (Storage(matrix.storage.size, storage)))

let foldAssociative (folder: 'T option -> 'T option -> 'T option) (state: 'T option) (matrix: SparseMatrix<'T>) =
    let rec traverse tree (size: uint64<storageSize>) (state: 'T option) =
        match tree with
        | Leaf Dummy -> state
        | Leaf(UserValue v) ->
            let area = (uint64 size) * (uint64 size)

            let rec foldValue size accum =
                if size = 1UL then
                    accum
                else
                    let halfSize = size / 2UL

                    foldValue halfSize (folder accum accum)

            folder state (foldValue area v)
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

let slice
    (matrix: SparseMatrix<'a>)
    (rowStart: int)
    (rowEnd: int)
    (colStart: int)
    (colEnd: int)
    : Result<SparseMatrix<'a>, string> =
    if rowStart < 0 then
        Error "Start row should be >= 0"
    elif rowEnd < 0 then
        Error "End row should be >= 0"
    elif colStart < 0 then
        Error "Start column should be >= 0"
    elif colEnd < 0 then
        Error "End column should be >= 0"
    elif rowStart > int matrix.nrows - 1 then
        Error "Start row is out of matrix length"
    elif rowEnd > int matrix.nrows - 1 then
        Error "End row is out of matrix length"
    elif colStart > int matrix.ncols - 1 then
        Error "Start column is out of matrix length"
    elif colEnd > int matrix.ncols - 1 then
        Error "End column is out of matrix length"
    elif rowStart > rowEnd then
        Error "Start row should be <= end row"
    elif colStart > colEnd then
        Error "Start column should be <= end column"
    else
        let rowStartIdx = uint64 rowStart * 1UL<rowindex>
        let rowEndIdx = uint64 rowEnd * 1UL<rowindex>
        let colStartIdx = uint64 colStart * 1UL<colindex>
        let colEndIdx = uint64 colEnd * 1UL<colindex>
        let newRows = uint64 (rowEnd - rowStart + 1) * 1UL<nrows>
        let newCols = uint64 (colEnd - colStart + 1) * 1UL<ncols>

        let newSize =
            getNearestUpperPowerOfTwo (max (uint64 newRows) (uint64 newCols))
            * 1UL<storageSize>

        let rec cut (size: uint64<storageSize>) (row: uint64<rowindex>) (col: uint64<colindex>) tree =
            let sizeRow = (uint64 size) * 1UL<rowindex>
            let sizeCol = (uint64 size) * 1UL<colindex>

            match tree with
            | Node(nw, ne, sw, se) ->
                let half = size / 2UL
                let halfRow = (uint64 half) * 1UL<rowindex>
                let halfCol = (uint64 half) * 1UL<colindex>

                mkNode
                    (cut half row col nw)
                    (cut half row (col + halfCol) ne)
                    (cut half (row + halfRow) col sw)
                    (cut half (row + halfRow) (col + halfCol) se)
            | Leaf(Dummy) -> Leaf Dummy
            | Leaf(UserValue(v)) when
                row >= rowStartIdx
                && row + sizeRow - 1UL<rowindex> <= rowEndIdx
                && col >= colStartIdx
                && col + sizeCol - 1UL<colindex> <= colEndIdx
                ->
                Leaf(UserValue(v))
            | _ -> Leaf Dummy

        let cutTree =
            cut matrix.storage.size 0UL<rowindex> 0UL<colindex> matrix.storage.data

        let rec empty (size: uint64<storageSize>) =
            match size with
            | 1UL<storageSize> -> Leaf Dummy
            | _ ->
                let half = size / 2UL
                let subtree = empty half
                Node(subtree, subtree, subtree, subtree)

        let rec insert (size: uint64<storageSize>) (row: uint64<rowindex>) (col: uint64<colindex>) value tree =
            match size with
            | 1UL<storageSize> -> Leaf(UserValue(value))
            | _ ->
                let half = size / 2UL
                let halfRow = (uint64 half) * 1UL<rowindex>
                let halfCol = (uint64 half) * 1UL<colindex>

                match tree with
                | Node(nw, ne, sw, se) ->
                    if row < halfRow then
                        if col < halfCol then
                            Node(insert half row col value nw, ne, sw, se)
                        else
                            Node(nw, insert half row (col - halfCol) value ne, sw, se)
                    else if col < halfCol then
                        Node(nw, ne, insert half (row - halfRow) col value sw, se)
                    else
                        Node(nw, ne, sw, insert half (row - halfRow) (col - halfCol) value se)
                | _ ->
                    let emptySub = empty half

                    if row < halfRow then
                        if col < halfCol then
                            Node(insert half row col value emptySub, emptySub, emptySub, emptySub)
                        else
                            Node(emptySub, insert half row (col - halfCol) value emptySub, emptySub, emptySub)
                    else if col < halfCol then
                        Node(emptySub, emptySub, insert half (row - halfRow) col value emptySub, emptySub)
                    else
                        Node(emptySub, emptySub, emptySub, insert half (row - halfRow) (col - halfCol) value emptySub)

        let rec rebuild (size: uint64<storageSize>) (row: uint64<rowindex>) (col: uint64<colindex>) tree acc =
            match tree with
            | Leaf(Dummy) -> acc
            | Leaf(UserValue(v)) ->
                let newRow = row - uint64 rowStart * 1UL<rowindex>
                let newCol = col - uint64 colStart * 1UL<colindex>
                insert newSize newRow newCol v acc
            | Node(nw, ne, sw, se) ->
                let half = size / 2UL
                let halfRow = (uint64 half) * 1UL<rowindex>
                let halfCol = (uint64 half) * 1UL<colindex>
                let acc = rebuild half row col nw acc
                let acc = rebuild half row (col + halfCol) ne acc
                let acc = rebuild half (row + halfRow) col sw acc
                rebuild half (row + halfRow) (col + halfCol) se acc

        let emptyTree = empty newSize

        let shiftedTree =
            rebuild matrix.storage.size 0UL<rowindex> 0UL<colindex> cutTree emptyTree

        let rec count (size: uint64<storageSize>) tree =
            match tree with
            | Node(nw, ne, sw, se) ->
                let half = size / 2UL
                count half nw + count half ne + count half sw + count half se
            | Leaf(UserValue(_)) -> 1UL<nvals>
            | _ -> 0UL<nvals>

        let nvals = count newSize shiftedTree

        Ok(SparseMatrix(newRows, newCols, nvals, Storage(newSize, shiftedTree)))

let reduceRows (op: 'a option -> 'a option -> 'a option) (matrix: SparseMatrix<'a>) : Vector.SparseVector<'a> =
    let rows = matrix.nrows

    let rec inner (size: uint64<storageSize>) (row: uint64<rowindex>) (col: uint64<colindex>) tree acc =
        match tree with
        | Node(nw, ne, sw, se) ->
            let half = size / 2UL
            let halfRow = (uint64 half) * 1UL<rowindex>
            let halfCol = (uint64 half) * 1UL<colindex>
            let acc = inner half row col nw acc
            let acc = inner half row (col + halfCol) ne acc
            let acc = inner half (row + halfRow) col sw acc
            let acc = inner half (row + halfRow) (col + halfCol) se acc
            acc
        | Leaf(Dummy) -> acc
        | Leaf(UserValue(None)) -> acc
        | Leaf(UserValue(Some v)) -> (row, v) :: acc

    let pairs =
        inner matrix.storage.size 0UL<rowindex> 0UL<colindex> matrix.storage.data []

    let grouped =
        List.sortBy fst pairs
        |> List.groupBy fst
        |> List.map (fun (row, rowSet) -> row, rowSet |> List.map snd |> List.map Some)

    let reduced =
        grouped
        |> List.map (fun (row, values) ->
            match values with
            | [] -> row, None
            | head :: tail -> row, List.fold op head tail)

    let data =
        reduced
        |> List.choose (fun (row, v) ->
            match v with
            | None -> None
            | Some x -> Some(row, 0UL<colindex>, x))
        |> List.sort

    let vectorData =
        data |> List.map (fun (row, _, v) -> (uint64 row * 1UL<Vector.index>, v))

    Vector.fromCoordinateList (Vector.CoordinateList(uint64 rows * 1UL<Vector.dataLength>, vectorData))

let reduceCols (op: 'a option -> 'a option -> 'a option) (matrix: SparseMatrix<'a>) : Vector.SparseVector<'a> =
    let cols = matrix.ncols

    let rec inner (size: uint64<storageSize>) (row: uint64<rowindex>) (col: uint64<colindex>) tree acc =
        match tree with
        | Node(nw, ne, sw, se) ->
            let half = size / 2UL
            let halfRow = (uint64 half) * 1UL<rowindex>
            let halfCol = (uint64 half) * 1UL<colindex>
            let acc = inner half row col nw acc
            let acc = inner half row (col + halfCol) ne acc
            let acc = inner half (row + halfRow) col sw acc
            let acc = inner half (row + halfRow) (col + halfCol) se acc
            acc
        | Leaf(Dummy) -> acc
        | Leaf(UserValue(None)) -> acc
        | Leaf(UserValue(Some v)) -> (col, v) :: acc

    let pairs =
        inner matrix.storage.size 0UL<rowindex> 0UL<colindex> matrix.storage.data []

    let grouped =
        List.sortBy fst pairs
        |> List.groupBy fst
        |> List.map (fun (col, colSet) -> col, colSet |> List.map snd |> List.map Some)

    let reduced =
        grouped
        |> List.map (fun (col, values) ->
            match values with
            | [] -> col, None
            | head :: tail -> col, List.fold op head tail)

    let data =
        reduced
        |> List.choose (fun (col, v) ->
            match v with
            | None -> None
            | Some x -> Some(0UL<rowindex>, col, x))
        |> List.sort

    let vectorData =
        data |> List.map (fun (_, col, v) -> (uint64 col * 1UL<Vector.index>, v))

    Vector.fromCoordinateList (Vector.CoordinateList(uint64 cols * 1UL<Vector.dataLength>, vectorData))

let kroneckerProduct
    (matrix1: SparseMatrix<'a>)
    (matrix2: SparseMatrix<'b>)
    (f: 'a -> 'b -> 'c option)
    : Result<SparseMatrix<'c>, string> =
    let newRows = uint64 matrix1.nrows * uint64 matrix2.nrows * 1UL<nrows>
    let newCols = uint64 matrix1.ncols * uint64 matrix2.ncols * 1UL<ncols>

    let newSize =
        getNearestUpperPowerOfTwo (max (uint64 newRows) (uint64 newCols))
        * 1UL<storageSize>

    let rec empty (size: uint64<storageSize>) =
        match size with
        | 1UL<storageSize> -> Leaf Dummy
        | _ ->
            let half = size / 2UL
            let subtree = empty half
            Node(subtree, subtree, subtree, subtree)

    let rec insert (size: uint64<storageSize>) (row: uint64<rowindex>) (col: uint64<colindex>) value tree =
        match size with
        | 1UL<storageSize> -> Leaf(UserValue(value))
        | _ ->
            let half = size / 2UL
            let halfRow = (uint64 half) * 1UL<rowindex>
            let halfCol = (uint64 half) * 1UL<colindex>

            match tree with
            | Node(nw, ne, sw, se) ->
                match (row < halfRow, col < halfCol) with
                | true, true -> Node(insert half row col value nw, ne, sw, se)
                | true, false -> Node(nw, insert half row (col - halfCol) value ne, sw, se)
                | false, true -> Node(nw, ne, insert half (row - halfRow) col value sw, se)
                | false, false -> Node(nw, ne, sw, insert half (row - halfRow) (col - halfCol) value se)
            | _ ->
                let emptySub = empty half

                match (row < halfRow, col < halfCol) with
                | true, true -> Node(insert half row col value emptySub, emptySub, emptySub, emptySub)
                | true, false -> Node(emptySub, insert half row (col - halfCol) value emptySub, emptySub, emptySub)
                | false, true -> Node(emptySub, emptySub, insert half (row - halfRow) col value emptySub, emptySub)
                | false, false ->
                    Node(emptySub, emptySub, emptySub, insert half (row - halfRow) (col - halfCol) value emptySub)

    let foldQuadtree folder state size tree =
        let rec inner rowOffset colOffset currentSize subTree acc =
            match subTree with
            | Leaf Dummy -> acc
            | Leaf(UserValue None) -> acc
            | Leaf(UserValue(Some value)) ->
                let rec loop currRow currCol currentAcc =
                    if currRow = rowOffset + currentSize then
                        currentAcc
                    elif currCol = colOffset + currentSize then
                        loop (currRow + 1UL) colOffset currentAcc
                    else
                        loop currRow (currCol + 1UL) (folder currentAcc currRow currCol value)

                loop rowOffset colOffset acc
            | Node(nw, ne, sw, se) ->
                let half = currentSize / 2UL
                let acc1 = inner rowOffset colOffset half nw acc
                let acc2 = inner rowOffset (colOffset + half) half ne acc1
                let acc3 = inner (rowOffset + half) colOffset half sw acc2
                inner (rowOffset + half) (colOffset + half) half se acc3

        inner 0UL 0UL (uint64 size) tree state

    let mat2Rows = uint64 matrix2.nrows
    let mat2Cols = uint64 matrix2.ncols
    let initialTree = empty newSize

    let foldMatrixB acc rowMat1 colMat1 valMat1 =
        foldQuadtree
            (fun currentAcc rowMat2 colMat2 valMat2 ->
                match f valMat1 valMat2 with
                | Some computedVal ->
                    let destRow = rowMat1 * mat2Rows + rowMat2
                    let destCol = colMat1 * mat2Cols + colMat2
                    insert newSize (destRow * 1UL<rowindex>) (destCol * 1UL<colindex>) (Some computedVal) currentAcc
                | None -> currentAcc)
            acc
            matrix2.storage.size
            matrix2.storage.data

    let finalTree =
        foldQuadtree
            (fun acc rowMat1 colMat1 valMat1 -> foldMatrixB acc rowMat1 colMat1 valMat1)
            initialTree
            matrix1.storage.size
            matrix1.storage.data

    let rec count (size: uint64<storageSize>) tree =
        match tree with
        | Node(nw, ne, sw, se) ->
            let half = size / 2UL
            count half nw + count half ne + count half sw + count half se
        | Leaf(UserValue(Some _)) -> 1UL<nvals>
        | _ -> 0UL<nvals>

    let nvals = count newSize finalTree

    Ok(SparseMatrix(newRows, newCols, nvals, Storage(newSize, finalTree)))
