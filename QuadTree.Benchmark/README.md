# Benchmarks

Benchmarking infrastructure for
* [BFS](BFS.fs) - Synchronous BFS using `bfs_level`
* [BFSAsync](BFSAsync.fs) - Parallel BFS using `bfs_levelAsync` with configurable max subtasks
* [SSSP](SSSP.fs)
* [Triangles counting](Triangles.fs)

## Steps to run

1. Add ```*.mtx``` files to the [```data```](data/) directory. Do not commit these files. Several small matrices are included for demo purposes only.
2. Configure ```MatrixName``` in the respective ```.fs``` files:  list the matrices to be used for evaluation in the  ```Params``` attribute. For example: ```[<Params("494_bus.mtx", "arc130.mtx")>]``` in ```BFS.fs```.
   ```fsharp
    [<Params("494_bus.mtx", "arc130.mtx")>]
    member val MatrixName = "" with get, set
   ```
3. Configure `MaxSubtasks` in BFSAsync.fs to specify maximum number of parallel subtasks allowed.
4. Ensure the matrix reader is correctly configured. In ```LoadMatrix ()``` , you can pass a boolean flag to ```readMtx``` indicating whether the matrix should be treated as a directed or undirected graph. Current configuration: 
   1. Undirected for ```BFS``` and ```Triangles counting```.
   2. Directed for ```SSSP```.
5. Run evaluation: ```dotnet run -c Release -- --filter '*.SSSP.*'``` You can use ```--filter``` to specify particular benchmarks. Use ```--filter '*'``` to run all available benchmarks.
6. Raw benchmarking results are saved in ```BenchmarkDotNet.Artifacts/results/*.csv```.
