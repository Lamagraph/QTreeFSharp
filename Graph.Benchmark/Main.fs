open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let benchmarks =
        BenchmarkSwitcher
            [| typeof<Graph.Benchmarks.BFS.Benchmark>
               typeof<Graph.Benchmarks.SSSP.Benchmark>
               typeof<Graph.Benchmarks.Triangles.Benchmark> |]

    benchmarks.Run argv |> ignore
    0
