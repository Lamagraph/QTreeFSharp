/// Triangle count: today's Graph.TriangleCount (baseline) against the three decoupled approaches on the quad-tree.
namespace Decoupling.Benchmarks.TriangleCount

open BenchmarkDotNet.Attributes
open Benchmark.Common.Utils
open Decoupling.Benchmarks

[<Config(typeof<MyConfig>)>]
type Benchmark() =

    let mutable matrix = Unchecked.defaultof<Matrix.SparseMatrix<float>>

    [<Params("494_bus.mtx", "arc130.mtx")>]
    member val MatrixName = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        matrix <- Setup.loadMatrix this.MatrixName

    [<Benchmark(Baseline = true)>]
    member _.Graph() =
        Graph.TriangleCount.triangleCount matrix

    [<Benchmark>]
    member _.A1() = A1.QuadTree.triangleCount matrix

    [<Benchmark>]
    member _.A1Bundled() =
        A1.Bundled.triangleCount A1.Bundled.QuadTree.triangleCount matrix

    [<Benchmark>]
    member _.A2() = A2.QuadTree.triangleCount matrix

    [<Benchmark>]
    member _.A3() = A3.QuadTree.triangleCount matrix
