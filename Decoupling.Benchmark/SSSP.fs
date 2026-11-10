/// SSSP from vertex 0: today's Graph.SSSP (baseline) against the three decoupled approaches on the quad-tree.
namespace Decoupling.Benchmarks.SSSP

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
    member _.Graph() = Graph.SSSP.sssp matrix 0UL

    [<Benchmark>]
    member _.A1() =
        A1.QuadTree.sssp matrix 0UL<Vector.index>

    [<Benchmark>]
    member _.A1Bundled() =
        A1.Bundled.sssp A1.Bundled.QuadTree.sssp matrix 0UL<Vector.index>

    [<Benchmark>]
    member _.A2() =
        A2.QuadTree.sssp matrix 0UL<Vector.index>

    [<Benchmark>]
    member _.A3() =
        A3.QuadTree.sssp matrix 0UL<Vector.index>
