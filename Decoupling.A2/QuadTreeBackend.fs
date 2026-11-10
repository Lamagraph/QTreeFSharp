/// Quad-tree instances of the A2 per-algorithm records. Note how `Vector.map2`/`Vector.mapi`
/// are wired once per algorithm.
module A2.QuadTreeBackend

let bfs<'b, 'c when 'c: equality>
    : BFS.Backend<'b, 'c, Vector.SparseVector<'c>, Matrix.SparseMatrix<'b>, Vector.Error, LinearAlgebra.Error> =
    { nvals = fun v -> v.nvals
      mapi = Vector.mapi
      map2 = Vector.map2
      mask = Vector.mask
      vxmiValues = LinearAlgebra.vxmi_values }

let sssp: SSSP.Backend<Vector.SparseVector<float>, Matrix.SparseMatrix<float>, Vector.Error, LinearAlgebra.Error> =
    { length = fun v -> v.length
      nvals = fun v -> v.nvals
      map2 = Vector.map2
      fromCoordinates = fun length entries -> Vector.fromCoordinateList (Vector.CoordinateList(length, entries))
      ncols = fun m -> m.ncols
      vxm = LinearAlgebra.vxm }

let triangleCount<'a when 'a: equality>
    : TriangleCount.Backend<'a, Matrix.SparseMatrix<'a>, Matrix.SparseMatrix<uint64>, LinearAlgebra.Error, Matrix.Error> =
    { transpose = Matrix.transpose
      lowerTriangle = Matrix.getLowerTriangle
      mxm = LinearAlgebra.mxm
      mask = Matrix.mask
      foldAssociative = Matrix.foldAssociative }
