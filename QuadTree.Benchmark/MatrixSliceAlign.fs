namespace QuadTree.Benchmarks.MatrixSliceAlign

open System
open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils
open Matrix
open QuadTree

[<Config(typeof<MyConfig>)>]
[<MemoryDiagnoser>]
type Benchmark() =

    [<Params(4096)>]
    member val Size = 0 with get, set

    [<Params(0.001, 0.005, 0.01, 0.05, 0.1, 0.5)>]
    member val Density = 0.0 with get, set

    [<Params(0)>]
    member val Seed = 0 with get, set

    [<Params(2048)>]
    member val SliceSize = 0 with get, set

    [<Params(0, 512, 1024, 1536, 2048, 1)>]
    member val StartOffset = 0 with get, set

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

    member private this.SliceWithOffset(m: Matrix.SparseMatrix<double>) =
        let n = int m.nrows
        let start = this.StartOffset
        let last = start + this.SliceSize - 1

        if last >= n then
            failwithf "Slice out of bounds: start=%d, last=%d, n=%d" start last n

        match Matrix.slice m start last start last with
        | Ok res -> res
        | Error msg -> failwithf "Slice failed: %s" msg

    [<Benchmark>]
    member this.Slice() =
        this.SliceWithOffset(this.Matrix) |> ignore
