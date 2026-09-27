namespace QuadTree.Benchmarks.BFS

open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils

[<Config(typeof<MyConfig>)>]
type Benchmark() =

    let mutable matrix = Unchecked.defaultof<Matrix.SparseMatrix<double>>

    [<Params("494_bus.mtx", "arc130.mtx")>]
    member val MatrixName = "" with get, set

    [<GlobalSetup>]
    member this.LoadMatrix() =
        matrix <-
            match readMtx (System.IO.Path.Combine(DIR_WITH_MATRICES, this.MatrixName)) false with
            | Ok m -> m
            | Error msg -> failwith $"Failed to load matrix {this.MatrixName}: {msg}"

    [<Benchmark>]
    member this.BFS() =
        let startVerticesResult =
            Vector.CoordinateList((uint64 matrix.ncols) * 1UL<Vector.dataLength>, [ 0UL<Vector.index>, 1UL ])
            |> Vector.fromCoordinateList

        let startVertices =
            match startVerticesResult with
            | Ok v -> v
            | Error msg -> failwith $"Failed to create start vertices: {msg}"

        Graph.BFS.bfs_level matrix startVertices
