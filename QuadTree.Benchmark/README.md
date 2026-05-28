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

---

#### Benchmark Results Summary

Based on the benchmarking data, we can draw the following architectural conclusions:

**1. Single Element Operations (`Adding`, `Deleting`)**
Perform predictably well, showing characteristic logarithmic $O(\log N)$ scaling. Increasing the set size by a factor of 1,000 (from 100 to 100,000) only increases execution time by roughly 2.3x (from ~580 ns to ~1.3 μs). Memory allocations per operation are minimal and stable.

**2. Tree Traversal vs. Sequential Operations**
The `Traversal` optimization is highly situational and depends heavily on the ratio between set sizes:
* **Best Case ($A \gg B$):** When the primary set is large and the secondary set is small (e.g., $A=100,000, B=100$), `Traversal` is significantly faster. For instance, `Intersection` via traversal is ~4x faster (Ratio 0.26) than the sequential baseline.
* **Worst Case ($A \le B$):** When sets are of equal size or $B$ is larger, the traversal overhead drastically degrades performance, making it up to 40x slower than standard sequential operations.

**3. Parallel Execution (Parallel Slowdown)**
Currently, the multi-threaded implementation across all operations (`Union`, `Intersection`, `Difference`, `Symmetrical Difference`) suffers from a severe **parallel slowdown**. 
* Parallel execution is consistently **2x to 24x slower** than the single-threaded baseline.
* **Task Explosion & GC Pressure:** The recursive nature of the tasks generates tens of thousands of work items for larger trees (e.g., 71,300+ completed work items for $A=100k, B=10k$). The overhead of task scheduling, context switching, and massive memory allocations (up to 100MB+ causing heavy Garbage Collection) entirely negates the benefits of concurrent execution.