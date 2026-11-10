/// Section 7 variant: A1's small records grouped per algorithm, so a caller passes one value.
/// The algorithm bodies in `A1.BFS`/`A1.SSSP`/`A1.TriangleCount` are reused unchanged.
module A1.Bundled

open A1.Backend

type BfsBackend<'b, 'c, 'vec, 'mat, 'vErr, 'laErr> =
    { vec: VectorOps<'c, 'vec, 'vErr>
      vmask: VectorMaskOps<'c, 'vec, 'vec, 'vErr>
      kernel: VxmiValuesOps<'b, 'c, 'vec, 'mat, 'laErr> }

type SsspBackend<'vec, 'mat, 'vErr, 'laErr> =
    { vec: VectorOps<float, 'vec, 'vErr>
      build: VectorBuildOps<float, 'vec, 'vErr>
      shape: MatrixShapeOps<'mat>
      kernel: VxmOps<float, float, float, 'vec, 'mat, 'vec, 'laErr> }

type TriangleCountBackend<'a, 'matA, 'matC, 'laErr, 'mErr> =
    { graphOps: MatrixOps<'a, 'matA>
      countOps: MatrixOps<uint64, 'matC>
      mxm: MxmOps<'a, 'a, uint64, 'matA, 'matA, 'matC, 'laErr>
      mask: MatrixMaskOps<'a, 'matC, 'matA, 'mErr> }

let bfsLevel (backend: BfsBackend<'b, uint64, 'vec, 'mat, 'vErr, 'laErr>) graph startVertices =
    BFS.bfsLevelWith backend.vec backend.vmask backend.kernel graph startVertices

let bfsParent (backend: BfsBackend<'b, uint64, 'vec, 'mat, 'vErr, 'laErr>) graph startVertices =
    BFS.bfsParentWith backend.vec backend.vmask backend.kernel graph startVertices

let sssp (backend: SsspBackend<'vec, 'mat, 'vErr, 'laErr>) graph startVertex =
    SSSP.ssspWith backend.vec backend.build backend.shape backend.kernel graph startVertex

let triangleCount (backend: TriangleCountBackend<'a, 'matA, 'matC, 'laErr, 'mErr>) graph =
    TriangleCount.triangleCountWith backend.graphOps backend.countOps backend.mxm backend.mask graph

/// Bundles assembled from the quad-tree capability records.
module QuadTree =
    let bfs<'b, 'c when 'c: equality> : BfsBackend<'b, 'c, Vector.SparseVector<'c>, Matrix.SparseMatrix<'b>, _, _> =
        { vec = QuadTreeBackend.vector
          vmask = QuadTreeBackend.vectorMask
          kernel = QuadTreeBackend.vxmiValues }

    let sssp: SsspBackend<Vector.SparseVector<float>, Matrix.SparseMatrix<float>, _, _> =
        { vec = QuadTreeBackend.vector
          build = QuadTreeBackend.vectorBuild
          shape = QuadTreeBackend.matrixShape
          kernel = QuadTreeBackend.vxm }

    let triangleCount<'a when 'a: equality>
        : TriangleCountBackend<'a, Matrix.SparseMatrix<'a>, Matrix.SparseMatrix<uint64>, _, _> =
        { graphOps = QuadTreeBackend.matrix
          countOps = QuadTreeBackend.matrix
          mxm = QuadTreeBackend.mxm
          mask = QuadTreeBackend.matrixMask }
