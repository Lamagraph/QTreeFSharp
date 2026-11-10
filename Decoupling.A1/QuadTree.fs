/// Quad-tree entry points with the same shape as today's `Graph.*` functions: the generic
/// algorithm applied to the quad-tree records. Kept in a sibling module (proposal question 5)
/// so the algorithm modules never reference the quad-tree.
module A1.QuadTree

let bfsLevel (graph: Matrix.SparseMatrix<'b>) (startVertices: Vector.SparseVector<uint64>) =
    BFS.bfsLevelWith QuadTreeBackend.vector QuadTreeBackend.vectorMask QuadTreeBackend.vxmiValues graph startVertices

let bfsParent (graph: Matrix.SparseMatrix<'b>) (startVertices: Vector.SparseVector<uint64>) =
    BFS.bfsParentWith QuadTreeBackend.vector QuadTreeBackend.vectorMask QuadTreeBackend.vxmiValues graph startVertices

let sssp (graph: Matrix.SparseMatrix<float>) (startVertex: uint64<Vector.index>) =
    SSSP.ssspWith
        QuadTreeBackend.vector
        QuadTreeBackend.vectorBuild
        QuadTreeBackend.matrixShape
        QuadTreeBackend.vxm
        graph
        startVertex

let triangleCount (graph: Matrix.SparseMatrix<'a>) =
    TriangleCount.triangleCountWith
        QuadTreeBackend.matrix
        QuadTreeBackend.matrix
        QuadTreeBackend.mxm
        QuadTreeBackend.matrixMask
        graph
