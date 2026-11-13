/// Quad-tree specialization of the generic `Graph.TriangleCount`.
/// Usage: `open Graph.Impl.QuadTree.TriangleCount`, then call e.g. `triangleCount` without an ops record.
module Graph.Impl.QuadTree.TriangleCount

type Error = Graph.TriangleCount.Error<LinearAlgebra.Error, Matrix.Error>

let triangleCountOps<'edge when 'edge: equality>
    : Graph.TriangleCount.TriangleCountOps<
          'edge,
          Matrix.SparseMatrix<'edge>,
          Matrix.SparseMatrix<uint64>,
          LinearAlgebra.Error,
          Matrix.Error
       > =
    { lowerTriangle = Matrix.getLowerTriangle
      transpose = Matrix.transpose
      mxm = LinearAlgebra.mxm
      mask = Matrix.mask
      foldAssociative = Matrix.foldAssociative }

let triangleCount (graph: Matrix.SparseMatrix<'edge>) =
    Graph.TriangleCount.triangleCount triangleCountOps graph
