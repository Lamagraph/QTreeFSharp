/// Helpers shared by the COO vector and matrix: joins and reductions over keyed lists.
///
/// Sorted vs unsorted: every COO operation has a sorted form (inputs and output sorted by key,
/// keys unique) and an `...Unsorted` twin (any order, keys still unique). The type does not say
/// which form a value is in, so mixing them up is not caught by the compiler:
/// - a sorted operation fed unsorted data silently produces duplicate keys and wrong values
///   (see `mergeUnion`);
/// - `...Unsorted` reductions fold in list order, so `op_add` must be associative and commutative
///   for the result not to depend on that order (e.g. a "first value wins" `op_add`, as in
///   `bfsParent`, picks a different but still valid parent);
/// - structural equality (`=`) on two unsorted values with the same entries may be `false`;
///   `sort` both first.
module LinearAlgebra.COO.Common

/// Union of two keyed lists, both sorted ascending by key with unique keys, in one linear pass.
/// The result is sorted by key.
/// Pitfall: on unsorted input, e.g. `[(2, a); (0, b)]` and `[(0, c)]`, it yields
/// `[(0, None, Some c); (2, Some a, None); (0, Some b, None)]`: key 0 is duplicated and
/// `a`/`c` are never paired. Use `joinUnion` for unsorted input.
let mergeUnion (xs: ('key * 'a) list) (ys: ('key * 'b) list) : ('key * 'a option * 'b option) list =
    let rec go xs ys acc =
        match xs, ys with
        | [], [] -> List.rev acc
        | (kx, a) :: xt, [] -> go xt [] ((kx, Some a, None) :: acc)
        | [], (ky, b) :: yt -> go [] yt ((ky, None, Some b) :: acc)
        | (kx, a) :: xt, (ky, b) :: yt ->
            let c = compare kx ky

            if c < 0 then go xt ys ((kx, Some a, None) :: acc)
            elif c > 0 then go xs yt ((ky, None, Some b) :: acc)
            else go xt yt ((kx, Some a, Some b) :: acc)

    go xs ys []

/// Union of two keyed lists in any order (keys unique within each list).
/// Entries of `xs` come first in their order, followed by the keys present only in `ys`.
let joinUnion (xs: ('key * 'a) list) (ys: ('key * 'b) list) : ('key * 'a option * 'b option) list =
    let yMap = Map.ofList ys
    let xKeys = xs |> List.map fst |> Set.ofList

    let fromXs = xs |> List.map (fun (k, a) -> k, Some a, Map.tryFind k yMap)

    let onlyYs =
        ys
        |> List.filter (fun (k, _) -> not (Set.contains k xKeys))
        |> List.map (fun (k, b) -> k, None, Some b)

    fromXs @ onlyYs

/// Applies `f` to every entry of a union. Keys present in the union get `f a b`; when
/// `f None None` is `Some`, every key of `allKeys` (sorted ascending) missing from the union
/// gets it too, so the result equals a dense application over all cells.
/// The result is sorted by key when `union` is, or whenever the dense fallback is taken.
let applyUnion
    (allKeys: unit -> 'key list)
    (f: 'a option -> 'b option -> 'c option)
    (union: ('key * 'a option * 'b option) list)
    : ('key * 'c) list =
    let results = union |> List.map (fun (k, a, b) -> k, f a b)

    match f None None with
    | None -> results |> List.choose (fun (k, r) -> r |> Option.map (fun c -> k, c))
    | Some _ as fill ->
        mergeUnion (List.sortBy fst results) (allKeys () |> List.map (fun k -> k, ()))
        |> List.choose (fun (k, r, _) -> (defaultArg r fill) |> Option.map (fun c -> k, c))

/// Groups `(key, value)` pairs by key and reduces each group with `op_add`, folding left in
/// input order. Groups appear in the order of their first occurrence; `None` results are dropped.
let groupReduce (op_add: 'v option -> 'v option -> 'v option) (pairs: ('key * 'v) list) : ('key * 'v) list =
    pairs
    |> List.groupBy fst
    |> List.choose (fun (k, group) ->
        group
        |> List.map (snd >> Some)
        |> List.reduce op_add
        |> Option.map (fun v -> k, v))

/// Removes duplicate keys keeping the last value, like the quad-tree's `fromCoordinateList`.
/// Keys appear in the order of their first occurrence.
let dedupLast (pairs: ('key * 'v) list) : ('key * 'v) list =
    pairs
    |> List.groupBy fst
    |> List.map (fun (k, group) -> k, snd (List.last group))
