namespace QuadTree.Benchmarks.Filters

open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils

[<Config(typeof<MyConfig>)>]
type Benchmark() =

    let mutable denseVec = Unchecked.defaultof<Vector.SparseVector<int>>
    let mutable sparseVec = Unchecked.defaultof<Vector.SparseVector<int>>
    let mutable denseMat = Unchecked.defaultof<Matrix.SparseMatrix<int>>
    let mutable sparseMat = Unchecked.defaultof<Matrix.SparseMatrix<int>>

    // [<Params(64, 256, 4096)>]
    [<Params(64)>]
    member val N = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let denseData = [ for i in 0UL .. uint64 this.N - 1UL -> (i * 1UL<Vector.index>, int i % 100) ]
        denseVec <- Vector.fromCoordinateList (Vector.CoordinateList(uint64 this.N * 1UL<Vector.dataLength>, denseData))

        let sparseData = [ for i in 0UL .. 10UL .. uint64 this.N - 1UL -> (i * 1UL<Vector.index>, int i % 100) ]
        sparseVec <- Vector.fromCoordinateList (Vector.CoordinateList(uint64 this.N * 1UL<Vector.dataLength>, sparseData))

        let denseMatData = [
            for r in 0UL .. uint64 this.N - 1UL do
                for c in 0UL .. uint64 this.N - 1UL do
                    (r * 1UL<Matrix.rowindex>, c * 1UL<Matrix.colindex>, int (r + c) % 100) ]
        denseMat <- Matrix.fromCoordinateList (Matrix.CoordinateList(uint64 this.N * 1UL<Matrix.nrows>, uint64 this.N * 1UL<Matrix.ncols>, denseMatData))

        let sparseMatData = [
            for r in 0UL .. 10UL .. uint64 this.N - 1UL do
                for c in 0UL .. 10UL .. uint64 this.N - 1UL do
                    (r * 1UL<Matrix.rowindex>, c * 1UL<Matrix.colindex>, int (r + c) % 100) ]
        sparseMat <- Matrix.fromCoordinateList (Matrix.CoordinateList(uint64 this.N * 1UL<Matrix.nrows>, uint64 this.N * 1UL<Matrix.ncols>, sparseMatData))

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
