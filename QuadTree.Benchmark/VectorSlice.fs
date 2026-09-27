namespace QuadTree.Benchmarks.VectorSlice

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

    [<Params(4000000, 5000000, 7500000, 10000000, 12500000)>]
    member val Size = 0 with get, set

    [<Params(0.01, 0.05, 0.1, 0.25, 0.5)>]
    member val Density = 0.0 with get, set

    [<Params(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)>]
    member val Seed = 0 with get, set

    member val Vector = Unchecked.defaultof<Vector.SparseVector<double>> with get, set

    member private this.GenerateVector(size: int, density: float, rng: Random) =
        let coords =
            [ for i in 0 .. size - 1 do
                  if rng.NextDouble() < density then
                      let value = double (rng.Next(1, 4))
                      yield (uint64 i * 1UL<Vector.index>, value) ]

        match Vector.fromCoordinateList (Vector.CoordinateList(uint64 size * 1UL<Vector.dataLength>, coords)) with
        | Ok v -> v
        | Error msg -> failwithf "Failed to create vector: %s" msg

    [<GlobalSetup>]
    member this.Setup() =
        let rng = Random(this.Seed)
        this.Vector <- this.GenerateVector(this.Size, this.Density, rng)

    member private this.SliceMiddle(v: Vector.SparseVector<double>) =
        let n = int v.length
        let start = n / 4
        let last = 3 * n / 4 - 1

        match Vector.slice start last v with
        | Ok res -> res
        | Error msg -> failwithf "Slice failed: %s" msg

    [<Benchmark>]
    member this.Slice() = this.SliceMiddle(this.Vector) |> ignore
