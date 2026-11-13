/// The default representation for graph algorithms, currently the quad-tree.
/// Usage: `open Graph.Impl.Default.BFS` (or `.SSSP`, `.TriangleCount`), then call e.g. `bfsLevel`.
/// Every `Graph.Impl.<Repr>` exposes the same module names, so a specific representation
/// is picked by `open Graph.Impl.<Repr>.<Alg>` with unchanged call sites.
/// Switching the default representation only requires changing this file.
///
/// Note: F# module abbreviations (`module BFS = Graph.Impl.QuadTree.BFS`) are private to
/// their declaration group and cannot be reached through `open`, hence the re-exports below.
namespace Graph.Impl.Default

module BFS =
    type Error = Graph.Impl.QuadTree.BFS.Error

    let bfsOps<'edge, 'vertex when 'vertex: equality> =
        Graph.Impl.QuadTree.BFS.bfsOps<'edge, 'vertex>

    let bfs op_add op_mult initVisited graph startVertices =
        Graph.Impl.QuadTree.BFS.bfs op_add op_mult initVisited graph startVertices

    let bfsLevel graph startVertices =
        Graph.Impl.QuadTree.BFS.bfsLevel graph startVertices

    let bfsParent graph startVertices =
        Graph.Impl.QuadTree.BFS.bfsParent graph startVertices

module SSSP =
    type Error = Graph.Impl.QuadTree.SSSP.Error

    let ssspOps = Graph.Impl.QuadTree.SSSP.ssspOps

    let sssp graph startVertex =
        Graph.Impl.QuadTree.SSSP.sssp graph startVertex

module TriangleCount =
    type Error = Graph.Impl.QuadTree.TriangleCount.Error

    let triangleCountOps<'edge when 'edge: equality> =
        Graph.Impl.QuadTree.TriangleCount.triangleCountOps<'edge>

    let triangleCount graph =
        Graph.Impl.QuadTree.TriangleCount.triangleCount graph
