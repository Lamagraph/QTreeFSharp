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
- **Sequential:** split/join-based recursive implementation (baseline).
- **Tree Traversal:** traverses one set and applies the operation element-by-element to the other.
- **Parallel:** same split/join recursion as Sequential, but run on multiple threads.
**Parameters:**
- `A`: size of the primary set — 100, 10,000, 100,000 (1,000 instead of 100 for the parallel and standard-library-comparison benchmarks).
- `B`: size of the secondary set — 100, 10,000, 100,000.
- `threads`: thread limit for parallel operations — 1, 2, 4.
**How to run AVLSet benchmarks:**
To run only the AVLSet benchmarks, use the following command:
`dotnet run -c Release --filter '*AVLSet*'`
 
---
 
### Benchmark results
 
**1. Single-element operations.** Adding or deleting an element only touches the nodes on the path from the root, so cost tracks the tree's height rather than its size: increasing $A$ from 100 to 100,000 (1,000×) increases execution time by only ~2.3× (582 ns $\to$ 1,377 ns) and allocation by about the same factor (880 B $\to$ 2,080 B), consistent with the tree staying balanced.
 
**2. Tree traversal vs. sequential.** The traversal-based implementations fold the corresponding tree operation into a copy of one set, once per element of the other (e.g. `Traversal.difference` copies $A$ and calls `remove` once per element of $B$), so cost is governed by the size of whichever set gets traversed, not by which one happens to be smaller.
- Traversed set is small — e.g. intersection at $A=100\,000$, $B=100$: ~3.8× faster than sequential (84 μs vs 318 μs).
- Traversed set is large — e.g. difference at $A=100$, $B=10\,000$: ~41× slower than sequential (6.96 ms vs 168 μs).

**3. Parallel.** The parallel implementation forks new tasks only while the current subtree's height is above a fixed threshold (10 in these benchmarks); below that it falls back to the sequential algorithm, which keeps the number of spawned tasks bounded regardless of set size. With 2 threads: union at $A=B=100\,000$ is ~1.39× faster than sequential (69.6 ms vs 96.9 ms), with essentially the same memory footprint (81.6 MB vs 80.7 MB); symmetrical difference at $A=B=100\,000$ is ~1.41× faster than sequential (70.2 ms vs 99 ms), also with essentially the same memory footprint (79.7 MB vs 78.8 MB), since parallel and sequential use the same split/join calls and no extra per-task buffers.
 
#### Table
 
| Operation (A × B) | Implementation | Time | Memory | Ratio | Note |
| --- | --- | --- | --- | --- | --- |
| **Single Add** (100) | Sequential | 582.1 ns | 880 B | 1.00 (base) | |
| **Single Add** (100,000) | Sequential | 1,376.7 ns | 2,080 B | ~2.3× | |
| --- | --- | --- | --- | --- | --- |
| **Intersection** (100k × 100) | Sequential | 318.25 μs | 447.82 KB | 1.00 (base) | |
| **Intersection** (100k × 100) | Tree Traversal | **83.99 μs** | **86.54 KB** | **~3.8× speedup** | $A \gg B$ |
| --- | --- | --- | --- | --- | --- |
| **Difference** (100 × 10k) | Sequential | 168.15 μs | 230.23 KB | 1.00 (base) | |
| **Difference** (100 × 10k) | Tree Traversal | 6,958.68 μs | 8.86 MB | **41.39× slower** | $A \ll B$ |
| --- | --- | --- | --- | --- | --- |
| **Union** (100k × 100k) | Sequential | 96.89 ms | 80.72 MB | 1.00 (base) | |
| **Union** (100k × 100k) | Parallel (2 threads) | **69.63 ms** | **81.61 MB** | **~1.39× speedup** | |
| --- | --- | --- | --- | --- | --- |
| **Symmetrical Difference** (100k × 100k) | Sequential | 99.04 ms | 78.83 MB | 1.00 (base) | |
| **Symmetrical Difference** (100k × 100k) | Parallel (2 threads) | **70.16 ms** | **79.7 MB** | **~1.41× speedup** | |