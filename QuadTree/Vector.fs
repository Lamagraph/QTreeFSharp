module Vector

open Common

type 'value btree =
    | Leaf of 'value treeValue
    | Node of 'value btree * 'value btree

[<Measure>]
type dataLength

[<Measure>]
type storageSize

[<Struct>]
type Storage<'value> =
    val size: uint64<storageSize>
    val data: btree<'value>
    new(_size, _data) = { size = _size; data = _data }

[<Struct>]
type SparseVector<'value> =
    val length: uint64<dataLength>
    val nvals: uint64<nvals>
    val storage: Storage<Option<'value>>

    new(_length, _nvals, _storage) =
        { length = _length
          nvals = _nvals
          storage = _storage }

type Error<'value1, 'value2> =
    | InconsistentStructureOfStorages of btree<Option<'value1>> * btree<Option<'value2>>
    | InconsistentSizeOfArguments of SparseVector<'value1> * SparseVector<'value2>

(*
let foldValues state f tree =
    match tree with
    | Leaf
*)


let mkNode t1 t2 =
    match (t1, t2) with
    | Leaf(v1), Leaf(v2) when v1 = v2 -> Leaf(v1)
    | _ -> Node(t1, t2)

[<Measure>]
type index

[<Struct>]
type CoordinateList<'value> =
    val length: uint64<dataLength>
    val data: (uint64<index> * 'value) list
    new(_length, _data) = { length = _length; data = _data }

let fromCoordinateList (lst: CoordinateList<'a>) : SparseVector<'a> =
    let length = lst.length
    let nvals = (uint64 <| List.length lst.data) * 1UL<nvals>
    let storageSize = (getNearestUpperPowerOfTwo <| uint64 length) * 1UL<storageSize>

    let rec traverse coordinates pointer size =
        match coordinates with
        | [] when uint64 (pointer + size) < uint64 (length) -> Leaf <| UserValue None, []
        | [] when uint64 pointer >= uint64 length -> Leaf Dummy, []
        | (idx, _) :: _ when idx > pointer + size -> Leaf <| UserValue None, coordinates
        | (idx, value) :: xs when idx = pointer && size = 1UL<index> -> Leaf << UserValue <| Some value, xs
        | _ ->
            let halfSize = size / 2UL

            let left, lCoordinates = traverse coordinates pointer halfSize
            let right, rCoordinates = traverse lCoordinates (pointer + halfSize) halfSize

            mkNode left right, rCoordinates

    let sortedCoordinates = List.sort lst.data

    let tree, _ =
        traverse sortedCoordinates 0UL<index> ((uint64 storageSize) * 1UL<index>)

    SparseVector(length, nvals, Storage(storageSize, tree))

let toCoordinateList (vector: SparseVector<'a>) =
    let length = vector.length

    let rec traverse tree accum (pointer: uint64<index>) (size: uint64<index>) =
        match tree with
        | Leaf Dummy
        | Leaf(UserValue(None)) -> accum
        | Leaf(UserValue(Some value)) ->
            accum
            @ [ for idx in 0UL .. uint64 (size - 1UL<index>) -> (pointer + idx * 1UL<index>, value) ]
        | Node(left, right) ->
            let halfSize = size / 2UL
            let lAccum = traverse left accum pointer halfSize
            let rAccum = traverse right lAccum (pointer + halfSize) halfSize
            rAccum

    let lst =
        traverse vector.storage.data [] 0UL<index> ((uint64 vector.storage.size) * 1UL<index>)

    CoordinateList(length, lst)

let map (vector: SparseVector<'a>) f =
    let rec inner (size: uint64<storageSize>) vector =
        match vector with
        | Node(x1, x2) ->
            let t1, nvals1 = inner (size / 2UL) x1
            let t2, nvals2 = inner (size / 2UL) x2
            (mkNode t1 t2), nvals1 + nvals2
        | Leaf(Dummy) -> Leaf(Dummy), 0UL<nvals>
        | Leaf(UserValue(v)) ->
            let res = f v

            let nnz =
                match res with
                | None -> 0UL<nvals>
                | _ -> (uint64 size) * 1UL<nvals>

            Leaf(UserValue(res)), nnz

    let storage, nvals = inner vector.storage.size vector.storage.data

    SparseVector(vector.length, nvals, (Storage(vector.storage.size, storage)))

let map2 (vector1: SparseVector<'a>) (vector2: SparseVector<'b>) f =
    let len1 = vector1.length

    let rec inner (size: uint64<storageSize>) vector1 vector2 =
        let _do x1 x2 y1 y2 =
            let new_size = size / 2UL

            match (inner new_size x1 y1), (inner new_size x2 y2) with
            | Result.Success((t1, nvals1)), Result.Success((t2, nvals2)) ->
                ((mkNode t1 t2), nvals1 + nvals2) |> Result.Success
            | Result.Failure(e), _
            | _, Result.Failure(e) -> Result.Failure(e)

        match (vector1, vector2) with
        | Node(x1, x2), Leaf(_) -> _do x1 x2 vector2 vector2
        | Leaf(_), Node(y1, y2) -> _do vector1 vector1 y1 y2
        | Node(x1, x2), Node(y1, y2) -> _do x1 x2 y1 y2
        | Leaf(Dummy), Leaf(Dummy) -> Result.Success(Leaf(Dummy), 0UL<nvals>)
        | Leaf(UserValue(v1)), Leaf(UserValue(v2)) ->
            let res = f v1 v2

            let nnz =
                match res with
                | None -> 0UL<nvals>
                | _ -> (uint64 size) * 1UL<nvals>

            Result.Success(Leaf(UserValue(res)), nnz)

        | (x, y) -> Result.Failure <| Error.InconsistentStructureOfStorages(x, y)

    if len1 = vector2.length then
        match inner vector1.storage.size vector1.storage.data vector2.storage.data with
        | Result.Failure(e) -> Result.Failure(e)
        | Result.Success((storage, nvals)) ->
            Result.Success(SparseVector(len1, nvals, (Storage(vector1.storage.size, storage))))
    else
        Result.Failure <| Error.InconsistentSizeOfArguments(vector1, vector2)

let mask (vector1: SparseVector<'a>) (vector2: SparseVector<'b>) f =
    map2 vector1 vector2 (fun v1 v2 -> if f v2 then v1 else None)
