namespace QuadTree.Benchmarks.SSSP

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
            | Error e -> failwith $"Failed to load matrix {this.MatrixName}: {e}"

    [<Benchmark>]
    member this.SSSP() = Graph.SSSP.sssp matrix 0UL
