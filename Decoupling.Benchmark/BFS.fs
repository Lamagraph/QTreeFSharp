/// Level BFS: today's Graph.BFS (baseline) against the three decoupled approaches on the quad-tree.
namespace Decoupling.Benchmarks.BFS

open BenchmarkDotNet.Attributes
open Benchmark.Common.Utils
open Decoupling.Benchmarks

[<Config(typeof<MyConfig>)>]
type Benchmark() =

    let mutable matrix = Unchecked.defaultof<Matrix.SparseMatrix<float>>
    let mutable start = Unchecked.defaultof<Vector.SparseVector<uint64>>

    [<Params("494_bus.mtx", "arc130.mtx")>]
    member val MatrixName = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        matrix <- Setup.loadMatrix this.MatrixName
        start <- Setup.startVertices matrix

    [<Benchmark(Baseline = true)>]
    member _.Graph() = Graph.BFS.bfsLevel matrix start

    [<Benchmark>]
    member _.A1() = A1.QuadTree.bfsLevel matrix start

    [<Benchmark>]
    member _.A1Bundled() =
        A1.Bundled.bfsLevel A1.Bundled.QuadTree.bfs matrix start

    [<Benchmark>]
    member _.A2() = A2.QuadTree.bfsLevel matrix start

    [<Benchmark>]
    member _.A3() = A3.QuadTree.bfsLevel matrix start
