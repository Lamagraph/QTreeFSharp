namespace QuadTree.Benchmarks.VectorSliceAlign

open System
open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils
open Vector
open Common

[<Config(typeof<MyConfig>)>]
[<MemoryDiagnoser>]
type Benchmark() =

    [<Params(4000000)>]
    member val Size = 0 with get, set

    [<Params(0.001, 0.005, 0.01, 0.05, 0.1, 0.5)>]
    member val Density = 0.0 with get, set

    [<Params(0)>]
    member val Seed = 0 with get, set

    [<Params(2000000)>]
    member val SliceSize = 0 with get, set

    [<Params(0, 1, 512)>]
    member val StartOffset = 0 with get, set

    member val Vector = Unchecked.defaultof<SparseVector<double>> with get, set

    member private this.GenerateVector(size: int, density: float, rng: Random) =
        let coords =
            [ for i in 0 .. size - 1 do
                  if rng.NextDouble() < density then
                      let value = double (rng.Next(1, 4))
                      yield (uint64 i * 1UL<index>, value) ]

        match Vector.fromCoordinateList (CoordinateList(uint64 size * 1UL<dataLength>, coords)) with
        | Ok v -> v
        | Error msg -> failwithf "Failed to create vector: %s" msg

    [<GlobalSetup>]
    member this.Setup() =
        let rng = Random(this.Seed)
        this.Vector <- this.GenerateVector(this.Size, this.Density, rng)

    member private this.SliceWithOffset(v: SparseVector<double>) =
        let n = int v.length
        let start = this.StartOffset
        let last = start + this.SliceSize - 1

        if last >= n then
            failwithf "Slice out of bounds: start=%d, last=%d, n=%d" start last n

        match Vector.slice start last v with
        | Ok res -> res
        | Error msg -> failwithf "Slice failed: %s" msg

    [<Benchmark>]
    member this.Slice() =
        this.SliceWithOffset(this.Vector) |> ignore
