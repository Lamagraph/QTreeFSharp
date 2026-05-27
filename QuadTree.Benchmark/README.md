# Benchmarks

Benchmarking infrastructure for
* [BFS](BFS.fs)
* [SSSP](SSSP.fs)
* [Triangles counting](Triangles.fs)
* [AVLSet](AVLSet.fs)

## Steps to run

1. Add ```*.mtx``` files to the [```data```](data/) directory. Do not commit these files. Several small matrices are included for demo purposes only.
2. Configure ```MatrixName``` in the respective ```.fs``` files:  list the matrices to be used for evaluation in the  ```Params``` attribute. For example: ```[<Params("494_bus.mtx", "arc130.mtx")>]``` in ```BFS.fs```.
   ```fsharp
    [<Params("494_bus.mtx", "arc130.mtx")>]
    member val MatrixName = "" with get, set
   ```
3. Ensure the matrix reader is correctly configured. In ```LoadMatrix ()``` , you can pass a boolean flag to ```readMtx``` indicating whether the matrix should be treated as a directed or undirected graph. Current configuration: undirected for all ```BFS```, ```SSSP``` and ```Triangles counting```.
4. Run evaluation: ```dotnet run -c Release -- --filter '*.SSSP.*'``` You can use ```--filter``` to specify particular benchmarks. Use ```--filter '*'``` to run all available benchmarks.
5. Raw benchmarking results are saved in ```BenchmarkDotNet.Artifacts/results/*.csv```.

### AVLSet

Benchmarking the `AVLSet` data structure operations.

**Tested operations:**
- `Adding` and `Deleting` single elements.
- Set operations: `Union`, `Intersection`, `Difference`, `Symmetrical Difference`.

For set operations, three implementations are compared:
- **Sequential:** Standard sequential operations (used as the Baseline).
- **Tree Traversal:** Optimized operations using tree traversal.
- **Parallel:** Multi-threaded operations.

**Parameters evaluated:**
- `A`: Size of the primary set (100; 10,000; 1,000,000).
- `B`: Size of the secondary set (100; 1,000; 100,000).
- `DataTypeA`: Data distribution for the primary set (`Random` or `Sorted`).
- `threads`: Number of threads allocated for parallel operations (1, 2, 4, 8).

**How to run AVLSet benchmarks:**
To run only the AVLSet benchmarks, use the following command:
`dotnet run -c Release --filter '*AVLSet*'`