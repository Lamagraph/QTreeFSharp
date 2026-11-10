/// Approach A3 (docs/decoupling-proposals.md, section 6): one record with every operation that
/// any algorithm uses. Each element-type/container combination is a type parameter:
///
/// - 'e, 'matE: graph edges and the graph matrix (BFS, TriangleCount)
/// - 'c, 'vecC: BFS frontier values and vectors of them
/// - 'matU: uint64 matrix of TriangleCount's path counts
/// - 'vecF, 'matF: float vectors and matrices of SSSP
/// - 'vErr, 'mErr, 'laErr: vector, matrix and linear-algebra errors
///
/// Adding an algorithm with another combination adds parameters and fields here and breaks every backend.
module A3.Backend

open Common

type Backend<'e, 'c, 'vecC, 'matE, 'matU, 'vecF, 'matF, 'vErr, 'mErr, 'laErr> =
    { // BFS
      vNvals: 'vecC -> uint64<nvals>
      vMapi: 'vecC -> (uint64<Vector.index> -> 'c option -> 'c option) -> 'vecC
      vMap2: 'vecC -> 'vecC -> ('c option -> 'c option -> 'c option) -> Result<'vecC, 'vErr>
      vMask: 'vecC -> 'vecC -> ('c option -> bool) -> Result<'vecC, 'vErr>
      vxmiValues:
          ('c option -> 'c option -> 'c option)
              -> (uint64<Vector.index> * 'c -> uint64<Matrix.rowindex> * uint64<Matrix.colindex> * 'e -> 'c option)
              -> 'vecC
              -> 'matE
              -> Result<'vecC, 'laErr>
      // TriangleCount
      mTranspose: 'matE -> 'matE
      mLowerTriangle: 'matE -> 'matE
      mMask: 'matU -> 'matE -> ('e option -> bool) -> Result<'matU, 'mErr>
      mFold: (uint64 option -> uint64 option -> uint64 option) -> uint64 option -> 'matU -> uint64 option
      mxm:
          (uint64 option -> uint64 option -> uint64 option)
              -> ('e option -> 'e option -> uint64 option)
              -> 'matE
              -> 'matE
              -> Result<'matU, 'laErr>
      // SSSP
      fLength: 'vecF -> uint64<Vector.dataLength>
      fNvals: 'vecF -> uint64<nvals>
      fMap2: 'vecF -> 'vecF -> (float option -> float option -> float option) -> Result<'vecF, 'vErr>
      fFromCoordinates: uint64<Vector.dataLength> -> (uint64<Vector.index> * float) list -> Result<'vecF, 'vErr>
      fNcols: 'matF -> uint64<Matrix.ncols>
      vxm:
          (float option -> float option -> float option)
              -> (float option -> float option -> float option)
              -> 'vecF
              -> 'matF
              -> Result<'vecF, 'laErr> }
