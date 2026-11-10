/// Sample graphs and printing helpers shared by the `Decoupling.A*` example programs.
module Samples

open Common

let private row (i: uint64) = i * 1UL<Matrix.rowindex>
let private col (j: uint64) = j * 1UL<Matrix.colindex>

/// 4-vertex weighted digraph from `Graph.Tests` (BFS and SSSP tests).
/// From vertex 0: BFS levels [0; 1; 1; 2], BFS parents [0; 0; 0; 1], shortest distances [0; 1; 1; 3].
let smallGraph: Matrix.COOEntry<float> list =
    [ row 0UL, col 1UL, 1.0
      row 0UL, col 2UL, 1.0
      row 1UL, col 0UL, 3.0
      row 1UL, col 2UL, 2.0
      row 1UL, col 3UL, 3.0
      row 2UL, col 3UL, 2.0
      row 3UL, col 2UL, 3.0 ]

let smallGraphSize = 4UL

/// Undirected 7-vertex graph from `Graph.Tests` (triangle count test); it has 5 triangles.
let triangleGraph: Matrix.COOEntry<unit> list =
    [ 0UL, 1UL
      0UL, 3UL
      1UL, 3UL
      1UL, 4UL
      1UL, 6UL
      2UL, 3UL
      2UL, 5UL
      2UL, 6UL
      3UL, 5UL
      3UL, 6UL
      4UL, 5UL
      4UL, 6UL ]
    |> List.collect (fun (i, j) -> [ row i, col j, (); row j, col i, () ])

let triangleGraphSize = 7UL

let matrix (size: uint64) (entries: Matrix.COOEntry<'a> list) =
    Matrix.fromCoordinateList (Matrix.CoordinateList(size * 1UL<Matrix.nrows>, size * 1UL<Matrix.ncols>, entries))

/// Vector of the given length with a single stored value at `vertex`.
let startVector (length: uint64) (vertex: uint64) (value: 'a) =
    Vector.fromCoordinateList (
        Vector.CoordinateList(length * 1UL<Vector.dataLength>, [ (vertex * 1UL<Vector.index>, value) ])
    )

let showVector (vector: Vector.SparseVector<'a>) =
    List.init (int vector.length) (fun i ->
        match Vector.unsafeGet vector (uint64 i * 1UL<Vector.index>) with
        | Some v -> sprintf "%O" v
        | None -> "_")
    |> String.concat "; "
    |> sprintf "[%s]"

/// Formats an algorithm result; `show` renders the success value, errors go through `ToString()`.
let showResult (show: 'a -> string) (result: Result<'a, 'e>) =
    match result with
    | Ok value -> show value
    | Error error -> sprintf "error: %O" error

let showCount (count: uint64 option) =
    count |> Option.map string |> Option.defaultValue "none"

/// Prints one example line: a title and its formatted outcome.
let report (title: string) (outcome: string) = printfn "  %-56s %s" title outcome
