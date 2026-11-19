/// COO specialization of the generic `Graph.SSSP`.
/// Usage: `open Graph.Impl.COO.SSSP`, then call e.g. `sssp` without an ops record.
module Graph.Impl.COO.SSSP

open LinearAlgebra.COO

type Error = Graph.SSSP.Error<Vector.Error, Operations.Error>

let ssspOps: Graph.SSSP.SsspOps<Vector.SparseVector<float>, Matrix.SparseMatrix<float>, Vector.Error, Operations.Error> =
    { nvals = Vector.nvals
      length = fun v -> v.length
      ncols = fun m -> m.ncols
      fromCoordinates = fun length data -> Vector.CoordinateList(length, data) |> Vector.fromCoordinateList
      map2 = Vector.map2
      vxm = Operations.vxm }

let sssp (graph: Matrix.SparseMatrix<float>) (startVertex: uint64) =
    Graph.SSSP.sssp ssspOps graph startVertex
