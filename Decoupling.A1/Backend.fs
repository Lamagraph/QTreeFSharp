/// Approach A1 (docs/decoupling-proposals.md, section 4): one small record per capability.
///
/// F# has neither higher-kinded types nor rank-2 record fields, so the container types
/// ('vec, 'mat), the element types and the error types are all parameters of the records.
/// An algorithm asks only for the records it uses.
module A1.Backend

open Common

/// Element-preserving vector basics.
type VectorOps<'a, 'vec, 'vErr> =
    { length: 'vec -> uint64<Vector.dataLength>
      nvals: 'vec -> uint64<nvals>
      mapi: 'vec -> (uint64<Vector.index> -> 'a option -> 'a option) -> 'vec
      map2: 'vec -> 'vec -> ('a option -> 'a option -> 'a option) -> Result<'vec, 'vErr> }

/// Building a vector from coordinates; split from `VectorOps` because only SSSP needs it.
type VectorBuildOps<'a, 'vec, 'vErr> =
    { fromCoordinates: uint64<Vector.dataLength> -> (uint64<Vector.index> * 'a) list -> Result<'vec, 'vErr> }

/// Masking `'vecA` by `'vecB`; the two vectors may have different element types.
type VectorMaskOps<'b, 'vecA, 'vecB, 'vErr> =
    { mask: 'vecA -> 'vecB -> ('b option -> bool) -> Result<'vecA, 'vErr> }

type MatrixShapeOps<'mat> =
    { nrows: 'mat -> uint64<Matrix.nrows>
      ncols: 'mat -> uint64<Matrix.ncols> }

/// Element-preserving matrix basics.
type MatrixOps<'a, 'mat> =
    { transpose: 'mat -> 'mat
      lowerTriangle: 'mat -> 'mat
      foldAssociative: ('a option -> 'a option -> 'a option) -> 'a option -> 'mat -> 'a option }

/// Masking `'matA` by `'matB`; the two matrices may have different element types.
type MatrixMaskOps<'b, 'matA, 'matB, 'mErr> =
    { mask: 'matA -> 'matB -> ('b option -> bool) -> Result<'matA, 'mErr> }

type MxmOps<'a, 'b, 'c, 'matA, 'matB, 'matC, 'laErr> =
    { mxm:
        ('c option -> 'c option -> 'c option)
            -> ('a option -> 'b option -> 'c option)
            -> 'matA
            -> 'matB
            -> Result<'matC, 'laErr> }

type VxmOps<'a, 'b, 'c, 'vecA, 'mat, 'vecC, 'laErr> =
    { vxm:
        ('c option -> 'c option -> 'c option)
            -> ('a option -> 'b option -> 'c option)
            -> 'vecA
            -> 'mat
            -> Result<'vecC, 'laErr> }

/// Index-aware vector-matrix product over stored values. Input and output vectors share 'vec,
/// because BFS feeds the result back as the next frontier.
type VxmiValuesOps<'b, 'c, 'vec, 'mat, 'laErr> =
    { vxmiValues:
        ('c option -> 'c option -> 'c option)
            -> (uint64<Vector.index> * 'c -> uint64<Matrix.rowindex> * uint64<Matrix.colindex> * 'b -> 'c option)
            -> 'vec
            -> 'mat
            -> Result<'vec, 'laErr> }
