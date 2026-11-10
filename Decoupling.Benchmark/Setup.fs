/// Inputs shared by all decoupling benchmarks. Matrices are read as undirected graphs, as in Graph.Benchmark.
module Decoupling.Benchmarks.Setup

open Benchmark.Common.Utils

let loadMatrix (name: string) =
    match readMtx (System.IO.Path.Combine(DIR_WITH_MATRICES, name)) false with
    | Ok m -> m
    | Error error -> failwith $"Failed to load matrix {name}: {error}"

/// Start vector for BFS: vertex 0 marked with 1UL, same as Graph.Benchmark.
let startVertices (matrix: Matrix.SparseMatrix<float>) =
    match
        Vector.CoordinateList((uint64 matrix.ncols) * 1UL<Vector.dataLength>, [ (0UL<Vector.index>, 1UL) ])
        |> Vector.fromCoordinateList
    with
    | Ok v -> v
    | Error error -> failwith $"Failed to create start vertices: {error}"
