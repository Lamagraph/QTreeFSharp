open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let benchmarks =
        BenchmarkSwitcher
            [| typeof<QuadTree.Benchmarks.BFS.Benchmark>
               typeof<QuadTree.Benchmarks.SSSP.Benchmark>
               typeof<QuadTree.Benchmarks.Triangles.Benchmark>
               typeof<QuadTree.Benchmarks.AVLSet.SingleOpsBenchmark>
               typeof<QuadTree.Benchmarks.AVLSet.TraversalSetsBenchmark>
               typeof<QuadTree.Benchmarks.AVLSet.ParallelSetsBenchmark>
               typeof<QuadTree.Benchmarks.AVLSet.FSSetsBenchmark> |]

    benchmarks.Run argv |> ignore
    0
