/// Quad-tree entry points with the same shape as today's `Graph.*` functions.
module A3.QuadTree

let bfsLevel (graph: Matrix.SparseMatrix<'e>) (startVertices: Vector.SparseVector<uint64>) =
    BFS.bfsLevelWith QuadTreeBackend.backend graph startVertices

let bfsParent (graph: Matrix.SparseMatrix<'e>) (startVertices: Vector.SparseVector<uint64>) =
    BFS.bfsParentWith QuadTreeBackend.backend graph startVertices

/// SSSP does not touch 'e and 'c, so they must be fixed explicitly; `unit` is an arbitrary choice.
let sssp (graph: Matrix.SparseMatrix<float>) (startVertex: uint64<Vector.index>) =
    SSSP.ssspWith QuadTreeBackend.backend<unit, unit> graph startVertex

/// TriangleCount does not touch the BFS frontier type 'c, so it is fixed explicitly.
let triangleCount (graph: Matrix.SparseMatrix<'e>) =
    TriangleCount.triangleCountWith QuadTreeBackend.backend<'e, unit> graph
