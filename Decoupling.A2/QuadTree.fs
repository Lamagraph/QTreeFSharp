/// Quad-tree entry points with the same shape as today's `Graph.*` functions.
module A2.QuadTree

let bfsLevel (graph: Matrix.SparseMatrix<'b>) (startVertices: Vector.SparseVector<uint64>) =
    BFS.bfsLevelWith QuadTreeBackend.bfs graph startVertices

let bfsParent (graph: Matrix.SparseMatrix<'b>) (startVertices: Vector.SparseVector<uint64>) =
    BFS.bfsParentWith QuadTreeBackend.bfs graph startVertices

let sssp (graph: Matrix.SparseMatrix<float>) (startVertex: uint64<Vector.index>) =
    SSSP.ssspWith QuadTreeBackend.sssp graph startVertex

let triangleCount (graph: Matrix.SparseMatrix<'a>) =
    TriangleCount.triangleCountWith QuadTreeBackend.triangleCount graph
