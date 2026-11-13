/// Quad-tree specialization of the generic `Graph.BFS`.
/// Usage: `open Graph.Impl.QuadTree.BFS`, then call e.g. `bfsLevel` without an ops record.
module Graph.Impl.QuadTree.BFS

type Error = Graph.BFS.Error<Vector.Error, LinearAlgebra.Error>

let bfsOps<'edge, 'vertex when 'vertex: equality>
    : Graph.BFS.BfsOps<
          'edge,
          'vertex,
          Vector.SparseVector<'vertex>,
          Matrix.SparseMatrix<'edge>,
          Vector.Error,
          LinearAlgebra.Error
       > =
    { nvals = fun v -> v.nvals
      mapi = Vector.mapi
      map2 = Vector.map2
      mask = Vector.mask
      vxmiValues = LinearAlgebra.vxmi_values }

let bfs op_add op_mult initVisited (graph: Matrix.SparseMatrix<'edge>) (startVertices: Vector.SparseVector<'vertex>) =
    Graph.BFS.bfs bfsOps op_add op_mult initVisited graph startVertices

let bfsLevel (graph: Matrix.SparseMatrix<'edge>) (startVertices: Vector.SparseVector<uint64>) =
    Graph.BFS.bfsLevel bfsOps graph startVertices

let bfsParent (graph: Matrix.SparseMatrix<'edge>) (startVertices: Vector.SparseVector<uint64>) =
    Graph.BFS.bfsParent bfsOps graph startVertices
