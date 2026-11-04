# QTreeFSharp

F# prototype of GraphBLAS-style sparse linear algebra on quad-trees, used to implement graph algorithms.

## Projects

- `QuadTree/` — library; see `QuadTree.fsproj` for compile order (`Common`, `Result`, `AVLSet`, `Vector`, `Matrix`, `LinearAlgebra`, then graph algorithms).
- `QuadTree.Tests/` — xUnit + FsUnit + FsCheck tests.
- `QuadTree.Benchmark/` — BenchmarkDotNet console app.

F# compiles files top-to-bottom: new source files must be added to the relevant `.fsproj` `<Compile Include=...>` list in dependency order, or they will silently not be compiled. Benchmark classes must also be registered in `QuadTree.Benchmark/Main.fs`'s `BenchmarkSwitcher`.

Requires .NET SDK 10 (`net10.0`).

## Commands

```bash
dotnet tool restore                  # once; installs pinned Fantomas 7.0.5
dotnet fantomas --check .            # CI format gate; runs before tests
dotnet fantomas .                    # apply formatting
dotnet build
dotnet test
dotnet test --filter "FullyQualifiedName~Graph.BFS"   # focused suite/namespace
```

CI (`.github/workflows/dotnet-tests.yaml`) fails on any formatting diff, so run Fantomas before considering work complete.

## Benchmarks

```bash
dotnet run -c Release --project QuadTree.Benchmark -- --filter '*BFS*'
```

- Matrices go in `QuadTree.Benchmark/data/`. `Utils.DIR_WITH_MATRICES` is a seven-level relative path chosen so it resolves from BenchmarkDotNet's generated working directory; do not "fix" it to a repo-relative path.
- `*.mtx` is gitignored, but the small demo matrices are committed. Use `git add -f` only when intentionally adding one.

## Conventions

- `Matrix` is a 2D quad-tree and `Vector` a 1D binary tree. Storage is padded to the next power of two independently of the logical dimensions.
- Preserve the `Leaf Dummy` vs `Leaf (UserValue None)` distinction: `Dummy` means an entirely empty/out-of-range subtree, `UserValue None` an in-range empty cell. Several algorithms branch on this.
- Dimensions and indices use F# units of measure (`nrows`, `ncols`, `nvals`, `rowindex`, `colindex`, `dataLength`, `index`, `storageSize`) and are `uint64`. Keep units on new APIs.
- Errors are returned as `Result<_, Error>` using the repo's own `Result` module and `resultM { ... }` computation expression (`QuadTree/Result.fs`); do not throw for expected failures.
- Graph algorithms are generic over `op_add`/`op_mult` (semiring-style); algorithm modules live under `Graph.*`, while `AVLSet` uses the `QuadTree.AVLSet` namespace with a parallel variant in `ParallelAVLSet.fs`.
