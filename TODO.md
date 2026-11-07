# TODOs

Always append, never delete, use Markdown's checklists.

Order isn't important.

- [x] Move algorithms to separate project.
- [x] Decide on project name and move `AVLSet.fs` to separate project for data structures required in algorithms, e.g. as matrix elements. (see GraphBLAS and LAGraph for inspiration).
- [x] Deprecate (or remove) `ParallelAVLSet.fs`.
- [ ] Check code for consistency, fix non-functional style in main code, remove dead code.
- [ ] Create documentation for existing quadtree representation.
- [ ] Implement COO matrix representation. Note that we need neither `Dummy` nor `Option` for storing values.
- [ ] Add units of measure to slicing functions (`Vector.slice`, `Matrix.slice`) and take `uint64` arguments instead of `int`.
- [ ] Rewrite `Matrix.kroneckerProduct` via `toCoordinateList` + `fromCoordinateList` to remove the imperative buffer/partition/tree-building scaffolding (`mutable`, `for` loops). Requires agreement with other maintainers before proceeding.
- [ ] Make `Matrix.reduceRows`/`reduceCols` functional: share a single pure grouping helper over `foldQuadtree` and remove `ResizeArray`/`mutable`/`for` loops. Note that today `op` returning `None` for every value in a bucket would throw on `acc.Value`; the rewrite should drop such entries instead.
