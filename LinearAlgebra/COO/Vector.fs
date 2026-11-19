/// Sparse vector in coordinate (COO) format: an immutable list of `(index, value)` pairs.
/// Only present values are stored; there is neither `Dummy` nor a stored `Option`.
///
/// Sorted form (all operations without a suffix): `data` is sorted by index, indices are unique
/// and `< length`. `...Unsorted` operations accept and return any order (indices still unique),
/// so a chain of them can skip intermediate sorting; `sort` returns to the sorted form.
/// See `LinearAlgebra.COO.Common` for the pitfalls of mixing the two forms.
module LinearAlgebra.COO.Vector

open LinearAlgebra.Common
open LinearAlgebra.COO.Common

/// Unvalidated input for `fromCoordinateList`: any order, duplicates allowed (last one wins).
[<Struct>]
type CoordinateList<'value> =
    val length: uint64<dataLength>
    val data: (uint64<index> * 'value) list
    new(_length, _data) = { length = _length; data = _data }

type SparseVector<'value> =
    { length: uint64<dataLength>
      data: (uint64<index> * 'value) list }

type Error =
    | InconsistentSizeOfArguments
    | CoordinatesOutOfRange

    override this.ToString() =
        match this with
        | InconsistentSizeOfArguments -> "Inconsistent size of arguments"
        | CoordinatesOutOfRange -> "Index out of range"

let nvals (vector: SparseVector<'a>) : uint64<nvals> =
    (uint64 (List.length vector.data)) * 1UL<nvals>

let empty (length: uint64<dataLength>) : SparseVector<'a> = { length = length; data = [] }

/// Brings a vector in any order into the sorted form.
let sort (vector: SparseVector<'a>) : SparseVector<'a> =
    { vector with
        data = List.sortBy fst vector.data }

/// Validates and deduplicates (last value wins) without sorting.
let fromCoordinateListUnsorted (lst: CoordinateList<'a>) : Result<SparseVector<'a>, Error> =
    if lst.data |> List.exists (fun (i, _) -> uint64 i >= uint64 lst.length) then
        Error CoordinatesOutOfRange
    else
        Ok
            { length = lst.length
              data = dedupLast lst.data }

let fromCoordinateList (lst: CoordinateList<'a>) : Result<SparseVector<'a>, Error> =
    fromCoordinateListUnsorted lst |> Result.map sort

/// Works for both forms; the order of the list is preserved.
let toCoordinateList (vector: SparseVector<'a>) : CoordinateList<'a> =
    CoordinateList(vector.length, vector.data)

/// Value at `index`, if any. Scans the whole list, so it works for both forms.
let unsafeGet (vector: SparseVector<'a>) (index: uint64<index>) : 'a option =
    vector.data |> List.tryFind (fun (i, _) -> i = index) |> Option.map snd

let private allIndices (length: uint64<dataLength>) () =
    List.init (int length) (fun i -> uint64 i * 1UL<index>)

let private mapiWith union (vector: SparseVector<'a>) (f: uint64<index> -> 'a option -> 'b option) =
    let data =
        union vector.data (allIndices vector.length () |> List.map (fun i -> i, ()))
        |> List.choose (fun (i, a, _) -> f i a |> Option.map (fun b -> i, b))

    { length = vector.length; data = data }

/// Applies `f` to every index, present or not (O(length)), like the quad-tree's `mapi`.
let mapi (vector: SparseVector<'a>) (f: uint64<index> -> 'a option -> 'b option) : SparseVector<'b> =
    mapiWith mergeUnion vector f

let mapiUnsorted (vector: SparseVector<'a>) (f: uint64<index> -> 'a option -> 'b option) : SparseVector<'b> =
    mapiWith joinUnion vector f

let private map2With union (vector1: SparseVector<'a>) (vector2: SparseVector<'b>) f =
    if vector1.length <> vector2.length then
        Error InconsistentSizeOfArguments
    else
        Ok
            { length = vector1.length
              data = union vector1.data vector2.data |> applyUnion (allIndices vector1.length) f }

/// Element-wise `f` over the union of stored indices. If `f None None` is `Some`, the
/// remaining indices are filled with it, so the result equals the quad-tree's `map2`.
let map2
    (vector1: SparseVector<'a>)
    (vector2: SparseVector<'b>)
    (f: 'a option -> 'b option -> 'c option)
    : Result<SparseVector<'c>, Error> =
    map2With mergeUnion vector1 vector2 f

let map2Unsorted
    (vector1: SparseVector<'a>)
    (vector2: SparseVector<'b>)
    (f: 'a option -> 'b option -> 'c option)
    : Result<SparseVector<'c>, Error> =
    map2With joinUnion vector1 vector2 f

/// Keeps the values of `vector1` where `f` holds for the corresponding cell of `vector2`.
let mask (vector1: SparseVector<'a>) (vector2: SparseVector<'b>) (f: 'b option -> bool) =
    map2 vector1 vector2 (fun v1 v2 -> if f v2 then v1 else None)

let maskUnsorted (vector1: SparseVector<'a>) (vector2: SparseVector<'b>) (f: 'b option -> bool) =
    map2Unsorted vector1 vector2 (fun v1 v2 -> if f v2 then v1 else None)
