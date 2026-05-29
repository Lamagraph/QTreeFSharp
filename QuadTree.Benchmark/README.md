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

### Benchmark Results

Based on the benchmarking data obtained via BenchmarkDotNet, we can draw comprehensive architectural conclusions regarding asymptotic complexity, algorithmic trade-offs, and multi-threading overhead in immutable data structures.

#### 1. Single Element Operations: Asymptotic Complexity Validation
Operations for single elements (`Adding`, `Deleting`) perfectly demonstrate the expected logarithmic **$O(\log N)$** time complexity associated with balanced AVL trees.
* **Execution Time:** Increasing the tree size by a factor of 1,000 (from $100$ to $100,000$ elements) only increases execution time by approximately **2.3x** (e.g., adding an element scales from $582.1$ ns to $1,376.7$ ns).
* **Memory Allocations:** Memory consumption also scales logarithmically. Adding an element to a tree of $100$ nodes allocates $880$ Bytes, while a tree of $100,000$ nodes requires only $2,080$ Bytes. This perfectly reflects the cost of path-copying (creating new nodes from the inserted leaf up to the root) in immutable tree structures.

#### 2. Tree Traversal vs. Sequential (Algorithmic Trade-offs)
The `Traversal` optimization yields highly polarized results depending on the specific operation and the ratio between the sizes of sets $A$ and $B$.

* **The Triumphs (`Intersection`):** Traversal completely dominates standard sequential intersection when set sizes are heavily skewed. For $A=100,000$ and $B=100$, Traversal takes **$83.9$ μs** compared to Sequential's **$318.2$ μs** (a **~3.8x speedup**). It bypasses deep recursive merges and instead maps the smaller set against the larger one efficiently.
* **The Catastrophes (`Difference` & `Symmetrical Difference`):** Traversal causes catastrophic algorithmic degradation for difference operations when applied to the wrong set ratios. For instance, calculating the Difference for $A=100$ and $B=10,000$ takes Sequential operations $168.1$ μs, while Traversal takes **$6,958.6$ μs** (a massive **41.3x slowdown**). This highlights the cost of blindly traversing a large tree to perform sequential lookups.
* **Conclusion:** The `Traversal` strategy should only be conditionally invoked using heuristics.

#### 3. The Parallel Slowdown Phenomenon
The multi-threaded implementation (`ParallelAVLSet`) was benchmarked across 1, 2, and 4 threads. Counter-intuitively, the parallel implementation consistently underperforms the sequential baseline across all metrics (time and memory), providing an example of **Parallel Slowdown**.

* **Task Explosion:** The recursive divide-and-conquer strategy spawns an excessive number of micro-tasks. In the `Intersection` benchmark ($A=100k, B=10k$), the parallel execution generated **$71,315$ completed work items** for a single operation. The overhead of scheduling and synchronizing these micro-tasks in the Thread Pool entirely eclipses the actual computational work.
* **Core Contention:** In almost all scenarios, allocating *more* threads worsened the execution time. For $A=10k, B=10k$ Difference, running on 1 thread took $16.7$ ms, but spreading it across 2 threads spiked the time to **$62.1$ ms**. This indicates severe CPU cache trashing, lock contention, and context switching penalties.
* **GC Thrashing & Memory Pressure:** Parallelizing immutable tree operations causes a severe allocation spike. In extreme cases, parallel operations allocated over **$100$ MB** of memory (compared to $18.4$ MB for Sequential), triggering over **17,000 Gen0 Garbage Collection cycles** in a single benchmark run. The GC "stop-the-world" pauses negate any multi-core benefits.
* **Architectural Takeaway:** Fine-grained parallelism is unsuited for lightweight immutable tree operations. To make parallelization viable in the future, a **Granularity Threshold** must be implemented (e.g., falling back to sequential execution for subtrees with fewer than $5,000$ nodes) to drastically reduce task overhead.

#### Table

| Operation Scenario (A × B) | Implementation Type | Execution Time | Memory Allocated | Ratio | Algorithmic Insight |
| --- | --- | --- | --- | --- | --- |
| **Single Add** (100) | Sequential Baseline | 582.1 ns | 880 B | 1.00 (Base) | Logarithmic $O(\log N)$ baseline. |
| **Single Add** (100,000) | Sequential Baseline | 1,376.7 ns | 2,080 B | ~2.3x scales | Expected path-copying cost. |
| --- | --- | --- | --- | --- | --- |
| **Intersection** (100k × 100) | Sequential Baseline | 318.25 μs | 447.82 KB | 1.00 (Base) | Standard recursive merge. |
| **Intersection** (100k × 100) | Tree Traversal | **83.99 μs** | **86.54 KB** | **~3.8x Speedup** | **Optimal Heuristic:** Huge $A \gg B$ asymmetry. |
| --- | --- | --- | --- | --- | --- |
| **Difference** (100 × 10k) | Sequential Baseline | 168.15 μs | 230.23 KB | 1.00 (Base) | Efficient baseline difference. |
| **Difference** (100 × 10k) | Tree Traversal | 6,958.68 μs | 8.86 MB | **41.39x Slowdown** | **Catastrophic Degradation:** Wrong tree ratio. |
| --- | --- | --- | --- | --- | --- |
| **Difference** (10k × 10k) | Sequential Baseline | 5.68 ms | 6.41 MB | 1.00 (Base) | Balanced trees sequential. |
| **Difference** (10k × 10k) | Parallel (4 Threads) | 57.65 ms | 26.66 MB | **10.28x Slowdown** | High Thread Pool & GC contention. |
| --- | --- | --- | --- | --- | --- |
| **Intersection** (100k × 10k) | Sequential Baseline | 16.29 ms | 18.45 MB | 1.00 (Base) | Large scale baseline. |
| **Intersection** (100k × 10k) | Parallel (4 Threads) | 357.84 ms | 103.17 MB | **22.03x Slowdown** | **Max Task Explosion:** 71,300+ tasks created. |