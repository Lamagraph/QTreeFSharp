open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let benchmarks =
        BenchmarkSwitcher
            [| typeof<QuadTree.Benchmarks.BFS.Benchmark>
               typeof<QuadTree.Benchmarks.SSSP.Benchmark>
               typeof<QuadTree.Benchmarks.Triangles.Benchmark>
               typeof<QuadTree.Benchmarks.ReduceComparison.Benchmark>
               typeof<QuadTree.Benchmarks.VectorSlice.Benchmark>
               typeof<QuadTree.Benchmarks.MatrixSlice.Benchmark>
               typeof<QuadTree.Benchmarks.Kronecker.Benchmark>
               typeof<QuadTree.Benchmarks.MatrixSliceAlign.Benchmark>
               typeof<QuadTree.Benchmarks.VectorSliceAlign.Benchmark> |]

    benchmarks.Run argv |> ignore
    0
