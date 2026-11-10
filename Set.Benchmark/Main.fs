open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let benchmarks =
        BenchmarkSwitcher
            [| typeof<Set.Benchmarks.AVLSet.SingleOpsBenchmark>
               typeof<Set.Benchmarks.AVLSet.TraversalSetsBenchmark>
               typeof<Set.Benchmarks.AVLSet.FSSetsBenchmark> |]

    benchmarks.Run argv |> ignore
    0
