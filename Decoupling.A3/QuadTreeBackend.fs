/// The single quad-tree instance of the A3 backend. It is generic in the graph element type 'e
/// and the BFS frontier type 'c; the other containers are fixed by the quad-tree.
module A3.QuadTreeBackend

open A3.Backend

let backend<'e, 'c when 'e: equality and 'c: equality>
    : Backend<
          'e,
          'c,
          Vector.SparseVector<'c>,
          Matrix.SparseMatrix<'e>,
          Matrix.SparseMatrix<uint64>,
          Vector.SparseVector<float>,
          Matrix.SparseMatrix<float>,
          Vector.Error,
          Matrix.Error,
          LinearAlgebra.Error
       > =
    { vNvals = fun v -> v.nvals
      vMapi = Vector.mapi
      vMap2 = Vector.map2
      vMask = Vector.mask
      vxmiValues = LinearAlgebra.vxmi_values
      mTranspose = Matrix.transpose
      mLowerTriangle = Matrix.getLowerTriangle
      mMask = Matrix.mask
      mFold = Matrix.foldAssociative
      mxm = LinearAlgebra.mxm
      fLength = fun v -> v.length
      fNvals = fun v -> v.nvals
      fMap2 = Vector.map2
      fFromCoordinates = fun length entries -> Vector.fromCoordinateList (Vector.CoordinateList(length, entries))
      fNcols = fun m -> m.ncols
      vxm = LinearAlgebra.vxm }
