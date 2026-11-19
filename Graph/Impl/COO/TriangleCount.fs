/// COO specialization of the generic `Graph.TriangleCount`.
/// Usage: `open Graph.Impl.COO.TriangleCount`, then call e.g. `triangleCount` without an ops record.
///
/// The ops record uses the sorted operations. The same computation can be fused by hand with
/// the `...Unsorted` twins, which never sort (`+` on `uint64` is commutative, so the
/// order-dependent reductions are safe here):
///
/// ```fsharp
/// let lower = Matrix.getLowerTriangle graph
/// Operations.mxmUnsorted op_add op_mult lower (Matrix.transposeUnsorted lower)
/// |> Result.bind (fun c -> Matrix.maskUnsorted c lower Option.isSome)
/// |> Result.map (Matrix.foldAssociativeUnsorted op_add None)
/// ```
module Graph.Impl.COO.TriangleCount

open LinearAlgebra.COO

type Error = Graph.TriangleCount.Error<Operations.Error, Matrix.Error>

let triangleCountOps<'edge>
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
