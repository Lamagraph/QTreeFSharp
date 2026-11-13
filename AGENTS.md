# QTreeFSharp

A F# sparse linear algebra suite used as a prototype for further translation to Interaction Nets interpreters such as [inpla](https://github.com/inpla/inpla), [bancai](https://github.com/Lamagraph/bancai) or [vine](https://github.com/VineLang/vine).

## Research questions

- RQ1: Which sparse matrix representation is most suitable (performant/easy to use) for Interaction Nets?

## Philosophy

- Algorithms must be decoupled from concrete matrix representation.
- In general, every representation must have a vector and a matrix implemented.
- Matrix operations must be decently fast, but writing C-like code for the sake of performance is discouraged.
- Functional-style constructions are preferred to OOP ones. E.g. passing a record with functions is preferred to implementing an interface.

## Coding Conventions

- Units of measure must be used for a clear distinction between dimensions and indices. They are usually of type `uint64`. Keep them on new APIs.
- Graph algorithms are generic over `op_add`/`op_mult` (semiring-style).
- Algorithm modules live under `Graph.*`.
  Algorithms should be generic over the representation: `Graph/<Alg>.fs` defines a `<Alg>Ops` record with exactly the representation operations the algorithm needs (one record per algorithm) and takes it as the first argument. Semiring operators usually stay in the algorithm, not in the ops record. Algorithm `Error` unions are generic over the inner error types. Ops records and generic algorithms must not name representation-specific types (`Vector.SparseVector`, `Vector.CoordinateList`, `Vector.Error`, ...); use neutral fields instead, e.g. `SsspOps.fromCoordinates` takes a plain `(index * value) list`. Only units of measure (`Vector.index`, `Matrix.ncols`, ...) are still taken from `QuadTree`, which is why `Graph` keeps its project reference.
- Representation specializations live in `Graph/Impl/<Repr>/<Alg>.fs` as modules `Graph.Impl.<Repr>.<Alg>` (e.g. `Graph.Impl.QuadTree.BFS`) and provide the ops instance plus the algorithm functions with ops applied. `Graph/Impl/Default.fs` (`namespace Graph.Impl.Default`) re-exports the default representation under the same module names. User code picks a representation per algorithm via the `open` and calls bare names: `open Graph.Impl.Default.BFS` (or `open Graph.Impl.QuadTree.BFS`), then `bfsLevel graph start`. The explicit `open` shadows the generic functions, which take an ops record first. F# module abbreviations are private to their declaration group and unreachable through `open`, so `Default.fs` re-exports each value by hand: when adding a public function or ops value to `Graph/Impl/<Repr>/<Alg>.fs`, add it to `Default.fs` too, and do not "simplify" `Default.fs` into `module X = ...` abbreviations.
- Type parameters in `Graph.*` algorithms and their ops records are descriptive camelCase names saying what the type holds (`'edge`, `'vertex`, `'graphMat`, `'countMat`, `'vec`, `'mat`, `'vErr`, `'laErr`), not single letters like `'a`/`'b`/`'c`. If a long function type then breaks Fantomas ("leads to invalid F# code", e.g. inside a record field), name it with a documented type abbreviation such as `Graph.BFS.IndexedMult` instead of shortening the parameter names.
- Set containers for algorithms reside in `Set`.
- Main code must not use imperative or OOP constructions like `for`-loops or classes, implementation must be as functionally idiomatic as possible. Test and benchmarks aren't restricted by this rule.
- OOP-style exceptions are prohibited. Use `Result<_, Error>` and `resultM { ... }` computation expression from `QuadTree/Result.fs`.
- Errors are per-module unions (`Vector.Error`, `Matrix.Error`, `LinearAlgebra.Error`, one per `Graph.*` algorithm) rather than a shared union or strings; outer errors wrap inner ones (`VectorAdditionProblem of Vector.Error`, `MXMProblem of LinearAlgebra.Error`, ...).
- Function and local binding names are camelCase; types and union cases are PascalCase. The deliberate exceptions are the `op_add`/`op_mult`/`op_min` semiring parameters and the `nw1_x_nw2`-style quadrant product names in `mxm`, where `_x_` marks the operand pair.
- When an `Error` case is reported to a user, override `ToString()` on the union so `%O`/`sprintf` yields a readable message instead of the union case name. This is done in `Vector.Error`, `Matrix.Error` and `MaggsPlotkinMST.Error`; the strings there are the ones previously delivered by exceptions, so keep their wording stable.

## Implementation Notes
(currently applies to quadtree representation)

- `Matrix` is a 2D quad-tree and `Vector` a 1D binary tree. Storage is padded to the next power of two independently of the logical dimensions.
- Preserve the `Leaf Dummy` vs `Leaf (UserValue None)` distinction: `Dummy` means an entirely empty/out-of-range subtree, `UserValue None` an in-range empty cell. Several algorithms branch on this.

## Projects

- `QuadTree/` — core library (representations, linear algebra); see `QuadTree.fsproj` for compile order.
- `QuadTree.Tests/` — xUnit + FsUnit + FsCheck tests for the core library.
- `QuadTree.Benchmark/` — BenchmarkDotNet console app for the core library.
- `Graph/` — graph algorithms built on the core library; see `Graph.fsproj` for compile order.
- `Graph.Tests/` — xUnit tests for graph algorithms.
- `Graph.Benchmark/` — BenchmarkDotNet console app for graph algorithms.
- `Set/` — reusable data structures (currently `AVLSet`); see `Set.fsproj` for compile order.
- `Set.Tests/` — xUnit + FsUnit + FsCheck tests for reusable data structures.
- `Set.Benchmark/` — BenchmarkDotNet console app for reusable data structures.
- `Benchmark.Common/` — shared benchmark utilities (matrix reader, `MyConfig`) used by `QuadTree.Benchmark` and `Graph.Benchmark`.

F# compiles files top-to-bottom: new source files must be added to the relevant `.fsproj` `<Compile Include=...>` list in dependency order, or they will silently not be compiled. Benchmark classes must also be registered in the relevant benchmark project's `Main.fs` `BenchmarkSwitcher` (`QuadTree.Benchmark`, `Graph.Benchmark` or `Set.Benchmark`).

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
dotnet run -c Release --project Set.Benchmark -- --filter '*AVLSet*'
dotnet run -c Release --project Graph.Benchmark -- --filter '*BFS*'
```

- Matrices go in the shared `data/` folder at the repo root. `Benchmark.Common.Utils.DIR_WITH_MATRICES` is an eight-level relative path chosen so it resolves from BenchmarkDotNet's child-process working directory (`<project>/bin/<config>/<tfm>/<Assembly>-<Job>-N/bin/<config>/<tfm>`, always eight levels below the repo root for top-level projects); do not "fix" it to a repo-relative path.
- `*.mtx` is gitignored, but the small demo matrices are committed. Use `git add -f` only when intentionally adding one.

## User workflow

Repository owner's rely heavily on agentic workflow.

If you are a human: try to maintain documentation for this project, simplifying agent's life.

If you are an agent: remember user to review changes and update AGENTS.md, TODO.md, in-code documentation after successful completion of a task and help them to do so.
