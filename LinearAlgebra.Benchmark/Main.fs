open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let benchmarks =
        BenchmarkSwitcher
            [| typeof<LinearAlgebra.Benchmarks.ReduceComparison.Benchmark>
               typeof<LinearAlgebra.Benchmarks.VectorSlice.Benchmark>
               typeof<LinearAlgebra.Benchmarks.MatrixSlice.Benchmark>
               typeof<LinearAlgebra.Benchmarks.Kronecker.Benchmark>
               typeof<LinearAlgebra.Benchmarks.MatrixSliceAlign.Benchmark>
               typeof<LinearAlgebra.Benchmarks.VectorSliceAlign.Benchmark> |]

    benchmarks.Run argv |> ignore
    0
