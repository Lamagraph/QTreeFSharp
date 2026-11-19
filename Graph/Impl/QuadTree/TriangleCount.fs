/// Quad-tree specialization of the generic `Graph.TriangleCount`.
/// Usage: `open Graph.Impl.QuadTree.TriangleCount`, then call e.g. `triangleCount` without an ops record.
module Graph.Impl.QuadTree.TriangleCount

open LinearAlgebra.QuadTree

type Error = Graph.TriangleCount.Error<Operations.Error, Matrix.Error>

let triangleCountOps<'edge when 'edge: equality>
    : Graph.TriangleCount.TriangleCountOps<
          'edge,
          Matrix.SparseMatrix<'edge>,
          Matrix.SparseMatrix<uint64>,
          Operations.Error,
          Matrix.Error
       > =
    { lowerTriangle = Matrix.getLowerTriangle
      transpose = Matrix.transpose
      mxm = Operations.mxm
      mask = Matrix.mask
      foldAssociative = Matrix.foldAssociative }

let triangleCount (graph: Matrix.SparseMatrix<'edge>) =
    Graph.TriangleCount.triangleCount triangleCountOps graph
