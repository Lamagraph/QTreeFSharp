module Vector

type 'value btree =
    | Leaf of 'value
    | Node of 'value btree * 'value btree

[<Struct>]
type SparseVector<'value> =
    val length: uint
    val nvals: uint
    val storage: btree<Option<'value>>

    new(_length, _nvals, _storage) =
        { length = _length
          nvals = _nvals
          storage = _storage }

(*let nvals (vector:SparseVector<'a>) =
    let compute
    match vector.nvals with
    | Some(i) -> i
    | None -> 

let nvals vector =
    match vector with
    | SparseVector(_,nvals,_) -> nvals
    *)
(*
let foldValues state f tree =
    match tree with
    | Leaf
*)

let mkNode t1 t2 =
    match (t1, t2) with
    | Leaf(v1), Leaf(v2) when v1 = v2 -> Leaf(v1)
    | _ -> Node(t1, t2)

let map2 (vector1: SparseVector<'a>) (vector2: SparseVector<'b>) f =
    let len1 = vector1.length

    if len1 = vector2.length then
        let rec inner (len: uint) vector1 vector2 =
            match (vector1, vector2) with
            | Node(t1, t2), Leaf(_) ->
                let new_t1, nvals1 = inner (len / 2u) t1 vector2
                let new_t2, nvals2 = inner (len / 2u) t2 vector2
                (mkNode new_t1 new_t2), nvals1 + nvals2
            | Leaf(_), Node(t1, t2) ->
                let new_t1, nvals1 = inner (len / 2u) vector1 t1
                let new_t2, nvals2 = inner (len / 2u) vector1 t2
                (mkNode new_t1 new_t2), nvals1 + nvals2
            | Node(t1, t2), Node(t3, t4) ->
                let new_t1, nvals1 = inner (len / 2u) t1 t3
                let new_t2, nvals2 = inner (len / 2u) t2 t4
                (mkNode new_t1 new_t2), nvals1 + nvals2
            | Leaf(v1), Leaf(v2) -> Leaf(f v1 v2), len

        let storage, nvals = inner len1 vector1.storage vector2.storage
        Result.Success(SparseVector(len1, nvals, storage))
    else
        Result.Failure "The length of the vector1 must be equals to the length of the vector2."

let mask (vector1: SparseVector<'a>) (vector2: SparseVector<'b>) f =
    map2 vector1 vector2 (fun v1 v2 -> if f v2 then v1 else None)
