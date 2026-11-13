/// Quad-tree specialization of the generic `Graph.SSSP`.
/// Usage: `open Graph.Impl.QuadTree.SSSP`, then call e.g. `sssp` without an ops record.
module Graph.Impl.QuadTree.SSSP

type Error = Graph.SSSP.Error<Vector.Error, LinearAlgebra.Error>

let ssspOps
    : Graph.SSSP.SsspOps<Vector.SparseVector<float>, Matrix.SparseMatrix<float>, Vector.Error, LinearAlgebra.Error> =
    { nvals = fun v -> v.nvals
      length = fun v -> v.length
      ncols = fun m -> m.ncols
      fromCoordinates = fun length data -> Vector.CoordinateList(length, data) |> Vector.fromCoordinateList
      map2 = Vector.map2
      vxm = LinearAlgebra.vxm }

let sssp (graph: Matrix.SparseMatrix<float>) (startVertex: uint64) =
    Graph.SSSP.sssp ssspOps graph startVertex
