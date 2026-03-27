namespace QuadTree.Benchmarks.BFS

open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils

[<Config(typeof<MyConfig>)>]
type BenchmarkAsync() =

    let mutable matrix = Unchecked.defaultof<Matrix.SparseMatrix<double>>


    [<Params("494_bus.mtx", "arc130.mtx")>]
    member val MatrixName = "" with get, set

    [<Params(1, 2, 4, 8)>]
    member val MaxSubtasks = 0 with get, set

    [<GlobalSetup>]
    member this.LoadMatrix() =
        matrix <- readMtx (System.IO.Path.Combine(DIR_WITH_MATRICES, this.MatrixName)) false

    [<Benchmark>]
    member this.BFSAsync() =
        let startVertices =
            Vector.CoordinateList((uint64 matrix.ncols) * 1UL<Vector.dataLength>, [ 0UL<Vector.index>, 1UL ])
            |> Vector.fromCoordinateList

        Graph.BFS.bfs_levelAsync this.MaxSubtasks matrix startVertices