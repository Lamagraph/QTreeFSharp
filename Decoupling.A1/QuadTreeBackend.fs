/// Quad-tree instances of the A1 capability records: plain re-exports of `Vector`, `Matrix`
/// and `LinearAlgebra`. Each instance is a generic value, instantiated per element type at use.
module A1.QuadTreeBackend

open A1.Backend

let vector<'a when 'a: equality> : VectorOps<'a, Vector.SparseVector<'a>, Vector.Error> =
    { length = fun v -> v.length
      nvals = fun v -> v.nvals
      mapi = Vector.mapi
      map2 = Vector.map2 }

let vectorBuild<'a when 'a: comparison> : VectorBuildOps<'a, Vector.SparseVector<'a>, Vector.Error> =
    { fromCoordinates = fun length entries -> Vector.fromCoordinateList (Vector.CoordinateList(length, entries)) }

let vectorMask<'a, 'b when 'a: equality>
    : VectorMaskOps<'b, Vector.SparseVector<'a>, Vector.SparseVector<'b>, Vector.Error> =
    { mask = Vector.mask }

let matrixShape<'a> : MatrixShapeOps<Matrix.SparseMatrix<'a>> =
    { nrows = fun m -> m.nrows
      ncols = fun m -> m.ncols }

let matrix<'a when 'a: equality> : MatrixOps<'a, Matrix.SparseMatrix<'a>> =
    { transpose = Matrix.transpose
      lowerTriangle = Matrix.getLowerTriangle
      foldAssociative = Matrix.foldAssociative }

let matrixMask<'a, 'b when 'a: equality>
    : MatrixMaskOps<'b, Matrix.SparseMatrix<'a>, Matrix.SparseMatrix<'b>, Matrix.Error> =
    { mask = Matrix.mask }

let mxm<'a, 'b, 'c when 'a: equality and 'b: equality and 'c: equality>
    : MxmOps<'a, 'b, 'c, Matrix.SparseMatrix<'a>, Matrix.SparseMatrix<'b>, Matrix.SparseMatrix<'c>, LinearAlgebra.Error> =
    { mxm = LinearAlgebra.mxm }

let vxm<'a, 'b, 'c when 'c: equality>
    : VxmOps<'a, 'b, 'c, Vector.SparseVector<'a>, Matrix.SparseMatrix<'b>, Vector.SparseVector<'c>, LinearAlgebra.Error> =
    { vxm = LinearAlgebra.vxm }

let vxmiValues<'b, 'c when 'c: equality>
    : VxmiValuesOps<'b, 'c, Vector.SparseVector<'c>, Matrix.SparseMatrix<'b>, LinearAlgebra.Error> =
    { vxmiValues = LinearAlgebra.vxmi_values }
