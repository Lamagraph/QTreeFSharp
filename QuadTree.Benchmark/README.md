# Benchmarks

Benchmarking infrastructure for the core [QuadTree](../QuadTree/) library:
* [AVLSet](AVLSet.fs)
* [ReduceComparison](ReduceComparison.fs)
* [VectorSlice](VectorSlice.fs) / [MatrixSlice](MatrixSlice.fs)
* [Kronecker](Kronecker.fs)
* [VectorSliceAlign](VectorSliceAlign.fs) / [MatrixSliceAlign](MatrixSliceAlign.fs)

Graph algorithm benchmarks (BFS, SSSP, Triangles counting) live in the separate [Graph.Benchmark](../Graph.Benchmark/) project. Matrix loading utilities are shared via the [Benchmark.Common](../Benchmark.Common/) library.

## Steps to run

1. Add ```*.mtx``` files to the [```data```](../data/) directory at the repository root. Do not commit these files. Several small matrices are included for demo purposes only.
2. Configure ```MatrixName``` in the respective ```.fs``` files:  list the matrices to be used for evaluation in the  ```Params``` attribute. For example: ```[<Params("bcsstk01", "bcsstk02")>]``` in ```ReduceComparison.fs```.
   ```fsharp
    [<Params("bcsstk01", "bcsstk02")>]
    member val MatrixName = "" with get, set
   ```
3. Ensure the matrix reader is correctly configured. In ```LoadMatrix ()``` , you can pass a boolean flag to ```readMtx``` indicating whether the matrix should be treated as a directed or undirected graph.
4. Run evaluation: ```dotnet run -c Release --project QuadTree.Benchmark -- --filter '*.AVLSet.*'``` You can use ```--filter``` to specify particular benchmarks. Use ```--filter '*'``` to run all available benchmarks.
5. Raw benchmarking results are saved in ```BenchmarkDotNet.Artifacts/results/*.csv```.
