[![.NET Tests](https://github.com/Lamagraph/QTreeFSharp/actions/workflows/dotnet-tests.yaml/badge.svg)](https://github.com/Lamagraph/QTreeFSharp/actions/workflows/dotnet-tests.yaml)

# QTreeFSharp
Quad‑tree based linear algebra in F# for GraphBLAS‑style graph analysis. This is a prototype for implementations using interaction nets. An example of such an implementation using Inpla can be found in [this repository](https://github.com/Lamagraph/QTreeInpla).

## Benchmarks
Infrastructure for benchmarking the implemented algorithms is available in the [respective project](QuadTree.Benchmark/).

## Implemented Algorithms

* Single-source level BFS
* Single-source shortest path (SSSP)
* Triangles counting

## TODO
* [ ] Single-source parent BFS
* [ ] Multiple-source level BFS
* [ ] Multiple-source parent BFS
* [ ] PageRank 
* [ ] Single-source RPQ reachability
