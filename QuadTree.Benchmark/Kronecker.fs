namespace QuadTree.Benchmarks.Kronecker

open System
open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils

[<Config(typeof<MyConfig>)>]
[<MemoryDiagnoser>]
type Benchmark() =

    [<Params(150, 200, 250, 300)>]
    member val SizeA = 0 with get, set

    [<Params(150, 200, 250, 300)>]
    member val SizeB = 0 with get, set

    [<Params(0.005, 0.01, 0.05, 0.1)>]
    member val DensityB = 0.0 with get, set

    [<Params(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)>]
    member val Seed = 0 with get, set

    member val A = Unchecked.defaultof<Matrix.SparseMatrix<double>> with get, set
    member val B = Unchecked.defaultof<Matrix.SparseMatrix<double>> with get, set

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
        this.A <- this.GenerateMatrix(this.SizeA, 0.01, rng)
        this.B <- this.GenerateMatrix(this.SizeB, this.DensityB, rng)

    [<Benchmark>]
    member this.Kronecker() =
        match Matrix.kroneckerProduct this.A this.B (fun a b -> Some(a * b)) with
        | Ok res -> res
        | Error msg -> failwithf "Kronecker failed: %s" msg
        |> ignore
