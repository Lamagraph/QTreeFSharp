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

type Error<'value1, 'value2> = InconsistentSizeOfArguments of SparseVector<'value1> * SparseVector<'value2>


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
    let size = (getNearestUpperPowerOfTwo <| uint64 length) * 1UL<storageSize>

    let rec traverse coordinates pointer size =
        match coordinates with
        | [] -> Leaf <| UserValue None, []
        | (idx, _) :: _ when idx > pointer + size -> Leaf <| UserValue None, coordinates
        | (idx, value) :: xs when idx = pointer && size = 1UL<index> -> Leaf << UserValue <| Some value, xs
        | _ ->
            let halfSize = size / 2UL

            let left, lCoordinates = traverse coordinates pointer halfSize
            let right, rCoordinates = traverse lCoordinates (pointer + halfSize) halfSize

            mkNode left right, rCoordinates

    let sortedCoordinates = List.sort lst.data
    let tree, _ = traverse sortedCoordinates 0UL<index> ((uint64 size) * 1UL<index>)

    SparseVector(length, nvals, Storage(size, tree))

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

let map2 (vector1: SparseVector<'a>) (vector2: SparseVector<'b>) f =
    let len1 = vector1.length

    let rec inner (size: uint64<storageSize>) vector1 vector2 =
        match (vector1, vector2) with
        | Node(t1, t2), Leaf(_) ->
            let new_t1, nvals1 = inner (size / 2UL) t1 vector2
            let new_t2, nvals2 = inner (size / 2UL) t2 vector2
            (mkNode new_t1 new_t2), nvals1 + nvals2
        | Leaf(_), Node(t1, t2) ->
            let new_t1, nvals1 = inner (size / 2UL) vector1 t1
            let new_t2, nvals2 = inner (size / 2UL) vector1 t2
            (mkNode new_t1 new_t2), nvals1 + nvals2
        | Node(t1, t2), Node(t3, t4) ->
            let new_t1, nvals1 = inner (size / 2UL) t1 t3
            let new_t2, nvals2 = inner (size / 2UL) t2 t4
            (mkNode new_t1 new_t2), nvals1 + nvals2
        | Leaf(Dummy), Leaf(Dummy) -> Leaf(Dummy), 0UL<nvals>
        | Leaf(UserValue(v1)), Leaf(UserValue(v2)) ->
            let res = f v1 v2

            let nnz =
                match res with
                | None -> 0UL<nvals>
                | _ -> (uint64 size) * 1UL<nvals>

            Leaf(UserValue(res)), nnz

    if len1 = vector2.length then

        let storage, nvals =
            inner vector1.storage.size vector1.storage.data vector2.storage.data

        Result.Success(SparseVector(len1, nvals, (Storage(vector1.storage.size, storage))))
    else
        Result.Failure <| Error.InconsistentSizeOfArguments(vector1, vector2)

let mask (vector1: SparseVector<'a>) (vector2: SparseVector<'b>) f =
    map2 vector1 vector2 (fun v1 v2 -> if f v2 then v1 else None)
