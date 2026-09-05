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
* **Tasks:** Balanced recursive partitioning prevents the generation of excessive micro-tasks, significantly reducing thread pool scheduling overhead.
* **Thread Contention:** Improved cache locality and minimized context switching allow execution time to scale effectively with the thread count.
* **GC Thrashing:** Memory allocation rates are now strictly controlled (nearly matching the sequential baseline, e.g., ~81.6 MB vs ~80.7 MB for a $100k \times 100k$ operation), completely preventing Gen0 garbage collection thrashing.

#### Table

| Operation Scenario (A × B) | Implementation Type | Execution Time | Memory Allocated | Ratio | Algorithmic Insight |
| --- | --- | --- | --- | --- | --- |
| **Single Add** (100) | Sequential | 582.1 ns | 880 B | 1.00 (Base) | Logarithmic $O(\log N)$ algorithm. |
| **Single Add** (100,000) | Sequential | 1,376.7 ns | 2,080 B | ~2.3x scales | Expected path-copying cost. |
| --- | --- | --- | --- | --- | --- |
| **Intersection** (100k × 100) | Sequential | 318.25 μs | 447.82 KB | 1.00 (Base) | Standard recursive intersection. |
| **Intersection** (100k × 100) | Tree Traversal | **83.99 μs** | **86.54 KB** | **~3.8x Speedup** | Huge $A \gg B$ asymmetry. |
| --- | --- | --- | --- | --- | --- |
| **Difference** (100 × 10k) | Sequential | 168.15 μs | 230.23 KB | 1.00 (Base) | Standard recursive difference. |
| **Difference** (100 × 10k) | Tree Traversal | 6,958.68 μs | 8.86 MB | **41.39x Slowdown** | Tree traversal slowdown. |
| --- | --- | --- | --- | --- | --- |
| **Union** (100k figure× 100k) | Sequential | 96.89 ms | 80.72 MB | 1.00 (Base) | Standard recursive union. |
| **Union** (100k × 100k) | Parallel (2 Threads) | **69.63 ms** | **81.61 MB** | **~1.39x Speedup** | Optimized parallel algorithm. |