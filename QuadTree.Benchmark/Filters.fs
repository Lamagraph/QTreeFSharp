namespace QuadTree.Benchmarks.Filters

open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils

[<Config(typeof<MyConfig>)>]
type Benchmark() =

    let mutable denseVec = Unchecked.defaultof<Vector.SparseVector<int>>
    let mutable sparseVec = Unchecked.defaultof<Vector.SparseVector<int>>
    let mutable denseMat = Unchecked.defaultof<Matrix.SparseMatrix<int>>

    let cooVec (lst: Vector.CoordinateList<'a>) : Vector.SparseVector<'a> =
        match Vector.fromCoordinateList lst with
        | Ok v -> v
        | Error e -> failwith e

    let cooMat (lst: Matrix.CoordinateList<'a>) : Matrix.SparseMatrix<'a> =
        match Matrix.fromCoordinateList lst with
        | Ok m -> m
        | Error e -> failwith e

    let mutable sparseMat = Unchecked.defaultof<Matrix.SparseMatrix<int>>

    [<Params(64, 256, 1024)>]
    member val N = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let denseData =
            [ for i in 0UL .. uint64 this.N - 1UL -> (i * 1UL<Vector.index>, int i % 100) ]

        denseVec <- cooVec (Vector.CoordinateList(uint64 this.N * 1UL<Vector.dataLength>, denseData))

        let sparseData =
            [ for i in 0UL .. 10UL .. uint64 this.N - 1UL -> (i * 1UL<Vector.index>, int i % 100) ]

        sparseVec <- cooVec (Vector.CoordinateList(uint64 this.N * 1UL<Vector.dataLength>, sparseData))

        let denseMatData =
            [ for r in 0UL .. uint64 this.N - 1UL do
                  for c in 0UL .. uint64 this.N - 1UL do
                      (r * 1UL<Matrix.rowindex>, c * 1UL<Matrix.colindex>, int (r + c) % 100) ]

        denseMat <-
            cooMat (
                Matrix.CoordinateList(
                    uint64 this.N * 1UL<Matrix.nrows>,
                    uint64 this.N * 1UL<Matrix.ncols>,
                    denseMatData
                )
            )

        let sparseMatData =
            [ for r in 0UL .. 10UL .. uint64 this.N - 1UL do
                  for c in 0UL .. 10UL .. uint64 this.N - 1UL do
                      (r * 1UL<Matrix.rowindex>, c * 1UL<Matrix.colindex>, int (r + c) % 100) ]

        sparseMat <-
            cooMat (
                Matrix.CoordinateList(
                    uint64 this.N * 1UL<Matrix.nrows>,
                    uint64 this.N * 1UL<Matrix.ncols>,
                    sparseMatData
                )
            )

    [<Benchmark>]
    member this.VectorFilterDense() =
        Vector.filter denseVec (fun x -> x % 2 = 0)

    [<Benchmark>]
    member this.VectorFilterSparse() =
        Vector.filter sparseVec (fun x -> x % 2 = 0)

    [<Benchmark>]
    member this.MatrixFilterDense() =
        Matrix.filter denseMat (fun x -> x % 2 = 0)

    [<Benchmark>]
    member this.MatrixFilterSparse() =
        Matrix.filter sparseMat (fun x -> x % 2 = 0)
