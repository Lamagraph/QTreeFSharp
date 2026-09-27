namespace QuadTree.Benchmarks.MatrixSlice

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open QuadTree.Benchmarks.Utils

type RealConfig() =
    inherit ManualConfig()
    do base.AddJob(Job.Default.WithWarmupCount(5).WithIterationCount(10)) |> ignore

[<Config(typeof<RealConfig>)>]
[<MemoryDiagnoser>]
type Benchmark() =

    [<Params(1000, 2000, 3000, 4000, 5000, 6000, 7000)>]
    member val Size = 0 with get, set

    [<Params(0.001, 0.005, 0.01, 0.05, 0.1, 0.5)>]
    member val Density = 0.0 with get, set

    [<Params(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)>]
    member val Seed = 0 with get, set

    member val Matrix = Unchecked.defaultof<Matrix.SparseMatrix<double>> with get, set

    member private this.GenerateMatrix(size: int, density: float, rng: Random) =
        let coords =
            [ for i in 0 .. size - 1 do
                  for j in 0 .. size - 1 do
                      if rng.NextDouble() < density then
                          let value = double (rng.Next(1, 4))
                          yield (uint64 i * 1UL<Matrix.rowindex>, uint64 j * 1UL<Matrix.colindex>, value) ]

        match
            Matrix.fromCoordinateList (
                Matrix.CoordinateList(uint64 size * 1UL<Matrix.nrows>, uint64 size * 1UL<Matrix.ncols>, coords)
            )
        with
        | Ok m -> m
        | Error msg -> failwithf "Failed to create matrix: %s" msg

    [<GlobalSetup>]
    member this.Setup() =
        let rng = Random(this.Seed)
        this.Matrix <- this.GenerateMatrix(this.Size, this.Density, rng)

    member private this.SliceMiddle(m: Matrix.SparseMatrix<double>) =
        let n = int m.nrows
        let start = n / 4
        let last = 3 * n / 4 - 1

        match Matrix.slice m start last start last with
        | Ok res -> res
        | Error msg -> failwithf "Slice failed: %s" msg

    [<Benchmark>]
    member this.Slice() = this.SliceMiddle(this.Matrix) |> ignore
