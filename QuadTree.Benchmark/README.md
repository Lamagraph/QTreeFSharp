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

### Benchmark results

#### 1. Single Element Operations
* **Time Complexity:** $O(\log N)$. Scaling tree size by 1,000x (100 $\rightarrow$ 100,000 nodes) increases execution time by only ~2.3x.
* **Memory Allocation:** Scales logarithmically due to standard path-copying overhead in immutable structures (880 B at 100 nodes $\rightarrow$ 2,080 B at 100,000 nodes).

#### 2. Traversal vs. Sequential Set Operations
Performance is strictly bound to the $|A| / |B|$ size ratio.
* **$|A| \gg |B|$:** `Traversal` is optimal. Yields ~3.8x speedup (e.g., Intersection, 100k $\times$ 100).
* **$|A| \ll |B|$:** `Traversal` is slow. Yields ~41x slowdown (e.g., Difference, 100 $\times$ 10k).

#### 3. Parallel Set Operations
The current `ParallelAVLSet` implementation exhibits parallel slowdown against the sequential one.
* **Tasks:** Recursive partitioning generates excessive micro-tasks (71k+ tasks for $100k \times 10k$ intersection).
* **Thread Contention:** Cache thrashing and context switching reduces execution time as thread count increases.
* **GC Thrashing:** High allocation rates (up to 103 MB per operation) trigger constant Gen0 collections.

#### Table

| Operation Scenario (A × B) | Implementation Type | Execution Time | Memory Allocated | Ratio | Algorithmic Insight |
| --- | --- | --- | --- | --- | --- |
| **Single Add** (100) | Sequential | 582.1 ns | 880 B | 1.00 (Base) | Logarithmic $O(\log N)$ algorithm. |
| **Single Add** (100,000) | Sequential | 1,376.7 ns | 2,080 B | ~2.3x scales | Expected path-copying cost. |
| --- | --- | --- | --- | --- | --- |
| **Intersection** (100k × 100) | Sequential | 318.25 μs | 447.82 KB | 1.00 (Base) | Standard recursive merge. |
| **Intersection** (100k × 100) | Tree Traversal | **83.99 μs** | **86.54 KB** | **~3.8x Speedup** | Huge $A \gg B$ asymmetry. |
| --- | --- | --- | --- | --- | --- |
| **Difference** (100 × 10k) | Sequential | 168.15 μs | 230.23 KB | 1.00 (Base) | Efficient difference algorithm. |
| **Difference** (100 × 10k) | Tree Traversal | 6,958.68 μs | 8.86 MB | **41.39x Slowdown** | Tree traversal slowdown. |
| --- | --- | --- | --- | --- | --- |
| **Difference** (10k × 10k) | Sequential | 5.68 ms | 6.41 MB | 1.00 (Base) | Balanced trees sequential. |
| **Difference** (10k × 10k) | Parallel (4 Threads) | 57.65 ms | 26.66 MB | **10.28x Slowdown** | High Thread Pool & GC contention. |
| --- | --- | --- | --- | --- | --- |
| **Intersection** (100k × 10k) | Sequential | 16.29 ms | 18.45 MB | 1.00 (Base) | Large scale algorithm. |
| **Intersection** (100k × 10k) | Parallel (4 Threads) | 357.84 ms | 103.17 MB | **22.03x Slowdown** | Parallel slowdown |