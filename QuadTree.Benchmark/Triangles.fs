namespace QuadTree.Benchmarks.Triangles

open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils

[<Config(typeof<MyConfig>)>]
type Benchmark() =

    let mutable matrix = Unchecked.defaultof<Matrix.SparseMatrix<double>>

    [<Params("494_bus.mtx", "arc130.mtx")>]
    member val MatrixName = "" with get, set

    [<GlobalSetup>]
    member this.LoadMatrix() =
        matrix <- readMtx (System.IO.Path.Combine(DIR_WITH_MATRICES, this.MatrixName)) false

    [<Benchmark>]
    member this.TriangleCount() =
        Graph.TriangleCount.triangle_count matrix
