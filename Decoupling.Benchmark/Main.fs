open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let benchmarks =
        BenchmarkSwitcher
            [| typeof<Decoupling.Benchmarks.BFS.Benchmark>
               typeof<Decoupling.Benchmarks.SSSP.Benchmark>
               typeof<Decoupling.Benchmarks.TriangleCount.Benchmark> |]

    benchmarks.Run argv |> ignore
    0
