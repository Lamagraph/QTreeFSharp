module Graph.BFS.Tests

open System
open Xunit

open Matrix
open Vector
open Common

let singleStart (n: uint64) (s: uint64) =
    Vector.CoordinateList(n * 1UL<Vector.dataLength>, [ s * 1UL<Vector.index>, 1UL ])
    |> Vector.fromCoordinateList

let vec (n: uint64) pairs =
    Vector.CoordinateList(
        n * 1UL<Vector.dataLength>,
        pairs |> List.map (fun (i: uint64, v: uint64) -> i * 1UL<Vector.index>, v)
    )
    |> Vector.fromCoordinateList

let unsafes (n: uint64) (v: Vector.SparseVector<_>) =
    List.init (int n) (fun i -> Vector.unsafeGet v (uint64 i * 1UL<Vector.index>))

[<Fact>]
let ``Simple level bfs.`` () =
    let graph =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(None))
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(3)))
                ),
                Matrix.qtree.Leaf(UserValue(None)),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(None))
                )
            )

        let store = Matrix.Storage(4UL<storageSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 9UL<nvals>, store)

    let startVertices =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1UL))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Leaf(UserValue(None))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(4UL<dataLength>, 1UL<nvals>, store)

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(0UL))), Vector.btree.Leaf(UserValue(Some(1UL)))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1UL))), Vector.btree.Leaf(UserValue(Some(2UL))))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        Ok(SparseVector(4UL<dataLength>, 4UL<nvals>, store))

    let actual = Graph.BFS.bfs_level graph startVertices

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple parent bfs.`` () =
    let graph =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(None))
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(3)))
                ),
                Matrix.qtree.Leaf(UserValue(None)),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(None))
                )
            )

        let store = Matrix.Storage(4UL<storageSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 9UL<nvals>, store)

    let startVertices =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1UL))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Leaf(UserValue(None))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(4UL<dataLength>, 1UL<nvals>, store)

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Leaf(UserValue(Some(0UL))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(0UL))), Vector.btree.Leaf(UserValue(Some(1UL))))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        Ok(SparseVector(4UL<dataLength>, 4UL<nvals>, store))

    let actual = Graph.BFS.bfs_parent graph startVertices

    Assert.Equal(expected, actual)

// ============== 3-node line ==============

let private line3graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 1UL
              1UL<rowindex>, 0UL<colindex>, 1UL
              1UL<rowindex>, 2UL<colindex>, 1UL
              2UL<rowindex>, 1UL<colindex>, 1UL ]
        )
    )

[<Fact>]
let ``Level bfs 3 node line start 0`` () =
    let n = uint64 line3graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level line3graph start
    let expected = [ Some 0UL; Some 1UL; Some 2UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 3 node line start 0`` () =
    let n = uint64 line3graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent line3graph start
    let expected = [ Some 0UL; Some 0UL; Some 1UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs 3 node line start 1`` () =
    let n = uint64 line3graph.ncols
    let start = singleStart n 1UL
    let result = Graph.BFS.bfs_level line3graph start
    let expected = [ Some 1UL; Some 0UL; Some 1UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 3 node line start 1`` () =
    let n = uint64 line3graph.ncols
    let start = singleStart n 1UL
    let result = Graph.BFS.bfs_parent line3graph start
    let expected = [ Some 1UL; Some 1UL; Some 1UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

// ============== 5-node star (center 0) ==============

let private star5graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            5UL<nrows>,
            5UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 1UL
              1UL<rowindex>, 0UL<colindex>, 1UL
              0UL<rowindex>, 2UL<colindex>, 1UL
              2UL<rowindex>, 0UL<colindex>, 1UL
              0UL<rowindex>, 3UL<colindex>, 1UL
              3UL<rowindex>, 0UL<colindex>, 1UL
              0UL<rowindex>, 4UL<colindex>, 1UL
              4UL<rowindex>, 0UL<colindex>, 1UL ]
        )
    )

[<Fact>]
let ``Level bfs 5 node star start center`` () =
    let n = uint64 star5graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level star5graph start
    let expected = [ Some 0UL; Some 1UL; Some 1UL; Some 1UL; Some 1UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 5 node star start center`` () =
    let n = uint64 star5graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent star5graph start
    let expected = [ Some 0UL; Some 0UL; Some 0UL; Some 0UL; Some 0UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs 5 node star start leaf`` () =
    let n = uint64 star5graph.ncols
    let start = singleStart n 1UL
    let result = Graph.BFS.bfs_level star5graph start
    let expected = [ Some 1UL; Some 0UL; Some 2UL; Some 2UL; Some 2UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 5 node star start leaf`` () =
    let n = uint64 star5graph.ncols
    let start = singleStart n 1UL
    let result = Graph.BFS.bfs_parent star5graph start
    let expected = [ Some 1UL; Some 1UL; Some 0UL; Some 0UL; Some 0UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

// ============== Two components ==============

let private twoCompGraph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            6UL<nrows>,
            6UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 1UL
              1UL<rowindex>, 0UL<colindex>, 1UL
              1UL<rowindex>, 2UL<colindex>, 1UL
              2UL<rowindex>, 1UL<colindex>, 1UL
              0UL<rowindex>, 2UL<colindex>, 1UL
              2UL<rowindex>, 0UL<colindex>, 1UL

              3UL<rowindex>, 4UL<colindex>, 1UL
              4UL<rowindex>, 3UL<colindex>, 1UL
              4UL<rowindex>, 5UL<colindex>, 1UL
              5UL<rowindex>, 4UL<colindex>, 1UL
              3UL<rowindex>, 5UL<colindex>, 1UL
              5UL<rowindex>, 3UL<colindex>, 1UL ]
        )
    )

[<Fact>]
let ``Level bfs two components start 0`` () =
    let n = uint64 twoCompGraph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level twoCompGraph start
    let expected = [ Some 0UL; Some 1UL; Some 1UL; None; None; None ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs two components start 0`` () =
    let n = uint64 twoCompGraph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent twoCompGraph start
    let expected = [ Some 0UL; Some 0UL; Some 0UL; None; None; None ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

// ============== Square (4-cycle) ==============

let private squareGraph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 1UL
              1UL<rowindex>, 0UL<colindex>, 1UL
              1UL<rowindex>, 2UL<colindex>, 2UL
              2UL<rowindex>, 1UL<colindex>, 2UL
              2UL<rowindex>, 3UL<colindex>, 1UL
              3UL<rowindex>, 2UL<colindex>, 1UL
              3UL<rowindex>, 0UL<colindex>, 2UL
              0UL<rowindex>, 3UL<colindex>, 2UL ]
        )
    )

[<Fact>]
let ``Level bfs square start 0`` () =
    let n = uint64 squareGraph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level squareGraph start
    let expected = [ Some 0UL; Some 1UL; Some 2UL; Some 1UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs square start 0`` () =
    let n = uint64 squareGraph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent squareGraph start
    let expected = [ Some 0UL; Some 0UL; Some 1UL; Some 0UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

// ============== 6-cycle ==============

let private cycle6graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            6UL<nrows>,
            6UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 1UL
              1UL<rowindex>, 0UL<colindex>, 1UL
              1UL<rowindex>, 2UL<colindex>, 2UL
              2UL<rowindex>, 1UL<colindex>, 2UL
              2UL<rowindex>, 3UL<colindex>, 3UL
              3UL<rowindex>, 2UL<colindex>, 3UL
              3UL<rowindex>, 4UL<colindex>, 4UL
              4UL<rowindex>, 3UL<colindex>, 4UL
              4UL<rowindex>, 5UL<colindex>, 5UL
              5UL<rowindex>, 4UL<colindex>, 5UL
              5UL<rowindex>, 0UL<colindex>, 6UL
              0UL<rowindex>, 5UL<colindex>, 6UL ]
        )
    )

[<Fact>]
let ``Level bfs 6 cycle start 0`` () =
    let n = uint64 cycle6graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level cycle6graph start
    let expected = [ Some 0UL; Some 1UL; Some 2UL; Some 3UL; Some 2UL; Some 1UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 6 cycle start 0`` () =
    let n = uint64 cycle6graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent cycle6graph start
    let expected = [ Some 0UL; Some 0UL; Some 1UL; Some 2UL; Some 5UL; Some 0UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

// ============== 2 nodes ==============

let private graph2 =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 5UL; 1UL<rowindex>, 0UL<colindex>, 5UL ]
        )
    )

[<Fact>]
let ``Level bfs 2 nodes start 0`` () =
    let n = uint64 graph2.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level graph2 start
    Assert.Equal(Ok [ Some 0UL; Some 1UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 2 nodes start 0`` () =
    let n = uint64 graph2.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent graph2 start
    Assert.Equal(Ok [ Some 0UL; Some 0UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs 2 nodes start 1`` () =
    let n = uint64 graph2.ncols
    let start = singleStart n 1UL
    let result = Graph.BFS.bfs_level graph2 start
    Assert.Equal(Ok [ Some 1UL; Some 0UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 2 nodes start 1`` () =
    let n = uint64 graph2.ncols
    let start = singleStart n 1UL
    let result = Graph.BFS.bfs_parent graph2 start
    Assert.Equal(Ok [ Some 1UL; Some 1UL ], Result.map (unsafes n) result)

// ============== 4-node line ==============

let private line4graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 1UL
              1UL<rowindex>, 0UL<colindex>, 1UL
              1UL<rowindex>, 2UL<colindex>, 2UL
              2UL<rowindex>, 1UL<colindex>, 2UL
              2UL<rowindex>, 3UL<colindex>, 3UL
              3UL<rowindex>, 2UL<colindex>, 3UL ]
        )
    )

[<Fact>]
let ``Level bfs 4 node line start 0`` () =
    let n = uint64 line4graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level line4graph start
    Assert.Equal(Ok [ Some 0UL; Some 1UL; Some 2UL; Some 3UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 4 node line start 0`` () =
    let n = uint64 line4graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent line4graph start
    Assert.Equal(Ok [ Some 0UL; Some 0UL; Some 1UL; Some 2UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs 4 node line start 3`` () =
    let n = uint64 line4graph.ncols
    let start = singleStart n 3UL
    let result = Graph.BFS.bfs_level line4graph start
    Assert.Equal(Ok [ Some 3UL; Some 2UL; Some 1UL; Some 0UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 4 node line start 3`` () =
    let n = uint64 line4graph.ncols
    let start = singleStart n 3UL
    let result = Graph.BFS.bfs_parent line4graph start
    Assert.Equal(Ok [ Some 1UL; Some 2UL; Some 3UL; Some 3UL ], Result.map (unsafes n) result)

// ============== 5-node line ==============

let private line5graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            5UL<nrows>,
            5UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 1UL
              1UL<rowindex>, 0UL<colindex>, 1UL
              1UL<rowindex>, 2UL<colindex>, 2UL
              2UL<rowindex>, 1UL<colindex>, 2UL
              2UL<rowindex>, 3UL<colindex>, 3UL
              3UL<rowindex>, 2UL<colindex>, 3UL
              3UL<rowindex>, 4UL<colindex>, 4UL
              4UL<rowindex>, 3UL<colindex>, 4UL ]
        )
    )

[<Fact>]
let ``Level bfs 5 node line start 0`` () =
    let n = uint64 line5graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level line5graph start
    Assert.Equal(Ok [ Some 0UL; Some 1UL; Some 2UL; Some 3UL; Some 4UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 5 node line start 0`` () =
    let n = uint64 line5graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent line5graph start
    Assert.Equal(Ok [ Some 0UL; Some 0UL; Some 1UL; Some 2UL; Some 3UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs 5 node line start 4`` () =
    let n = uint64 line5graph.ncols
    let start = singleStart n 4UL
    let result = Graph.BFS.bfs_level line5graph start
    Assert.Equal(Ok [ Some 4UL; Some 3UL; Some 2UL; Some 1UL; Some 0UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 5 node line start 4`` () =
    let n = uint64 line5graph.ncols
    let start = singleStart n 4UL
    let result = Graph.BFS.bfs_parent line5graph start
    Assert.Equal(Ok [ Some 1UL; Some 2UL; Some 3UL; Some 4UL; Some 4UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs 5 node line start 2`` () =
    let n = uint64 line5graph.ncols
    let start = singleStart n 2UL
    let result = Graph.BFS.bfs_level line5graph start
    Assert.Equal(Ok [ Some 2UL; Some 1UL; Some 0UL; Some 1UL; Some 2UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 5 node line start 2`` () =
    let n = uint64 line5graph.ncols
    let start = singleStart n 2UL
    let result = Graph.BFS.bfs_parent line5graph start
    Assert.Equal(Ok [ Some 1UL; Some 2UL; Some 2UL; Some 2UL; Some 3UL ], Result.map (unsafes n) result)

// ============== Simple triangle ==============

let private triangleGraph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 1UL
              1UL<rowindex>, 0UL<colindex>, 1UL
              0UL<rowindex>, 2UL<colindex>, 1UL
              2UL<rowindex>, 0UL<colindex>, 1UL
              1UL<rowindex>, 2UL<colindex>, 1UL
              2UL<rowindex>, 1UL<colindex>, 1UL ]
        )
    )

[<Fact>]
let ``Level bfs triangle start 0`` () =
    let n = uint64 triangleGraph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level triangleGraph start
    Assert.Equal(Ok [ Some 0UL; Some 1UL; Some 1UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs triangle start 0`` () =
    let n = uint64 triangleGraph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent triangleGraph start
    Assert.Equal(Ok [ Some 0UL; Some 0UL; Some 0UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs triangle start 2`` () =
    let n = uint64 triangleGraph.ncols
    let start = singleStart n 2UL
    let result = Graph.BFS.bfs_level triangleGraph start
    Assert.Equal(Ok [ Some 1UL; Some 1UL; Some 0UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs triangle start 2`` () =
    let n = uint64 triangleGraph.ncols
    let start = singleStart n 2UL
    let result = Graph.BFS.bfs_parent triangleGraph start
    Assert.Equal(Ok [ Some 2UL; Some 2UL; Some 2UL ], Result.map (unsafes n) result)

// ============== 5-node complete graph ==============

let private complete5graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            5UL<nrows>,
            5UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 1UL
              1UL<rowindex>, 0UL<colindex>, 1UL
              0UL<rowindex>, 2UL<colindex>, 2UL
              2UL<rowindex>, 0UL<colindex>, 2UL
              0UL<rowindex>, 3UL<colindex>, 3UL
              3UL<rowindex>, 0UL<colindex>, 3UL
              0UL<rowindex>, 4UL<colindex>, 4UL
              4UL<rowindex>, 0UL<colindex>, 4UL
              1UL<rowindex>, 2UL<colindex>, 5UL
              2UL<rowindex>, 1UL<colindex>, 5UL
              1UL<rowindex>, 3UL<colindex>, 6UL
              3UL<rowindex>, 1UL<colindex>, 6UL
              1UL<rowindex>, 4UL<colindex>, 7UL
              4UL<rowindex>, 1UL<colindex>, 7UL
              2UL<rowindex>, 3UL<colindex>, 8UL
              3UL<rowindex>, 2UL<colindex>, 8UL
              2UL<rowindex>, 4UL<colindex>, 9UL
              4UL<rowindex>, 2UL<colindex>, 9UL
              3UL<rowindex>, 4UL<colindex>, 10UL
              4UL<rowindex>, 3UL<colindex>, 10UL ]
        )
    )

[<Fact>]
let ``Level bfs 5 node complete start 0`` () =
    let n = uint64 complete5graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level complete5graph start
    Assert.Equal(Ok [ Some 0UL; Some 1UL; Some 1UL; Some 1UL; Some 1UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 5 node complete start 0`` () =
    let n = uint64 complete5graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent complete5graph start
    Assert.Equal(Ok [ Some 0UL; Some 0UL; Some 0UL; Some 0UL; Some 0UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs 5 node complete start 4`` () =
    let n = uint64 complete5graph.ncols
    let start = singleStart n 4UL
    let result = Graph.BFS.bfs_level complete5graph start
    Assert.Equal(Ok [ Some 1UL; Some 1UL; Some 1UL; Some 1UL; Some 0UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 5 node complete start 4`` () =
    let n = uint64 complete5graph.ncols
    let start = singleStart n 4UL
    let result = Graph.BFS.bfs_parent complete5graph start
    Assert.Equal(Ok [ Some 4UL; Some 4UL; Some 4UL; Some 4UL; Some 4UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs 5 node complete start 2`` () =
    let n = uint64 complete5graph.ncols
    let start = singleStart n 2UL
    let result = Graph.BFS.bfs_level complete5graph start
    Assert.Equal(Ok [ Some 1UL; Some 1UL; Some 0UL; Some 1UL; Some 1UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 5 node complete start 2`` () =
    let n = uint64 complete5graph.ncols
    let start = singleStart n 2UL
    let result = Graph.BFS.bfs_parent complete5graph start
    Assert.Equal(Ok [ Some 2UL; Some 2UL; Some 2UL; Some 2UL; Some 2UL ], Result.map (unsafes n) result)

// ============== K3,3 bipartite ==============

let private k33graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            6UL<nrows>,
            6UL<ncols>,
            [ 0UL<rowindex>, 3UL<colindex>, 1UL
              3UL<rowindex>, 0UL<colindex>, 1UL
              0UL<rowindex>, 4UL<colindex>, 2UL
              4UL<rowindex>, 0UL<colindex>, 2UL
              0UL<rowindex>, 5UL<colindex>, 3UL
              5UL<rowindex>, 0UL<colindex>, 3UL
              1UL<rowindex>, 3UL<colindex>, 4UL
              3UL<rowindex>, 1UL<colindex>, 4UL
              1UL<rowindex>, 4UL<colindex>, 5UL
              4UL<rowindex>, 1UL<colindex>, 5UL
              1UL<rowindex>, 5UL<colindex>, 6UL
              5UL<rowindex>, 1UL<colindex>, 6UL
              2UL<rowindex>, 3UL<colindex>, 7UL
              3UL<rowindex>, 2UL<colindex>, 7UL
              2UL<rowindex>, 4UL<colindex>, 8UL
              4UL<rowindex>, 2UL<colindex>, 8UL
              2UL<rowindex>, 5UL<colindex>, 9UL
              5UL<rowindex>, 2UL<colindex>, 9UL ]
        )
    )

[<Fact>]
let ``Level bfs K3 3 start 0`` () =
    let n = uint64 k33graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level k33graph start
    Assert.Equal(Ok [ Some 0UL; Some 2UL; Some 2UL; Some 1UL; Some 1UL; Some 1UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs K3 3 start 0`` () =
    let n = uint64 k33graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent k33graph start
    Assert.Equal(Ok [ Some 0UL; Some 3UL; Some 3UL; Some 0UL; Some 0UL; Some 0UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs K3 3 start 5`` () =
    let n = uint64 k33graph.ncols
    let start = singleStart n 5UL
    let result = Graph.BFS.bfs_level k33graph start
    Assert.Equal(Ok [ Some 1UL; Some 1UL; Some 1UL; Some 2UL; Some 2UL; Some 0UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs K3 3 start 5`` () =
    let n = uint64 k33graph.ncols
    let start = singleStart n 5UL
    let result = Graph.BFS.bfs_parent k33graph start
    Assert.Equal(Ok [ Some 5UL; Some 5UL; Some 5UL; Some 0UL; Some 0UL; Some 5UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs K3 3 start 3`` () =
    let n = uint64 k33graph.ncols
    let start = singleStart n 3UL
    let result = Graph.BFS.bfs_level k33graph start
    Assert.Equal(Ok [ Some 1UL; Some 1UL; Some 1UL; Some 0UL; Some 2UL; Some 2UL ], Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs K3 3 start 3`` () =
    let n = uint64 k33graph.ncols
    let start = singleStart n 3UL
    let result = Graph.BFS.bfs_parent k33graph start
    Assert.Equal(Ok [ Some 3UL; Some 3UL; Some 3UL; Some 3UL; Some 0UL; Some 0UL ], Result.map (unsafes n) result)

// ============== 8 nodes random weights ==============

let private random8graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            8UL<nrows>,
            8UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 7UL
              1UL<rowindex>, 0UL<colindex>, 7UL
              0UL<rowindex>, 2UL<colindex>, 5UL
              2UL<rowindex>, 0UL<colindex>, 5UL
              0UL<rowindex>, 3UL<colindex>, 9UL
              3UL<rowindex>, 0UL<colindex>, 9UL
              1UL<rowindex>, 2UL<colindex>, 3UL
              2UL<rowindex>, 1UL<colindex>, 3UL
              1UL<rowindex>, 3UL<colindex>, 4UL
              3UL<rowindex>, 1UL<colindex>, 4UL
              2UL<rowindex>, 3UL<colindex>, 2UL
              3UL<rowindex>, 2UL<colindex>, 2UL
              4UL<rowindex>, 5UL<colindex>, 1UL
              5UL<rowindex>, 4UL<colindex>, 1UL
              4UL<rowindex>, 6UL<colindex>, 6UL
              6UL<rowindex>, 4UL<colindex>, 6UL
              4UL<rowindex>, 7UL<colindex>, 8UL
              7UL<rowindex>, 4UL<colindex>, 8UL
              5UL<rowindex>, 6UL<colindex>, 3UL
              6UL<rowindex>, 5UL<colindex>, 3UL
              5UL<rowindex>, 7UL<colindex>, 5UL
              7UL<rowindex>, 5UL<colindex>, 5UL
              6UL<rowindex>, 7UL<colindex>, 2UL
              7UL<rowindex>, 6UL<colindex>, 2UL
              3UL<rowindex>, 4UL<colindex>, 10UL
              4UL<rowindex>, 3UL<colindex>, 10UL ]
        )
    )

[<Fact>]
let ``Level bfs 8 node random start 0`` () =
    let n = uint64 random8graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level random8graph start

    Assert.Equal(
        Ok
            [ Some 0UL
              Some 1UL
              Some 1UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 3UL
              Some 3UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 8 node random start 0`` () =
    let n = uint64 random8graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent random8graph start

    Assert.Equal(
        Ok
            [ Some 0UL
              Some 0UL
              Some 0UL
              Some 0UL
              Some 3UL
              Some 4UL
              Some 4UL
              Some 4UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Level bfs 8 node random start 7`` () =
    let n = uint64 random8graph.ncols
    let start = singleStart n 7UL
    let result = Graph.BFS.bfs_level random8graph start

    Assert.Equal(
        Ok
            [ Some 3UL
              Some 3UL
              Some 3UL
              Some 2UL
              Some 1UL
              Some 1UL
              Some 1UL
              Some 0UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 8 node random start 7`` () =
    let n = uint64 random8graph.ncols
    let start = singleStart n 7UL
    let result = Graph.BFS.bfs_parent random8graph start

    Assert.Equal(
        Ok
            [ Some 3UL
              Some 3UL
              Some 3UL
              Some 4UL
              Some 7UL
              Some 7UL
              Some 7UL
              Some 7UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Level bfs 8 node random start 4`` () =
    let n = uint64 random8graph.ncols
    let start = singleStart n 4UL
    let result = Graph.BFS.bfs_level random8graph start

    Assert.Equal(
        Ok
            [ Some 2UL
              Some 2UL
              Some 2UL
              Some 1UL
              Some 0UL
              Some 1UL
              Some 1UL
              Some 1UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 8 node random start 4`` () =
    let n = uint64 random8graph.ncols
    let start = singleStart n 4UL
    let result = Graph.BFS.bfs_parent random8graph start

    Assert.Equal(
        Ok
            [ Some 3UL
              Some 3UL
              Some 3UL
              Some 4UL
              Some 4UL
              Some 4UL
              Some 4UL
              Some 4UL ],
        Result.map (unsafes n) result
    )

// ============== 8 nodes grid ==============

let private grid8graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            8UL<nrows>,
            8UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 1UL
              1UL<rowindex>, 0UL<colindex>, 1UL
              1UL<rowindex>, 2UL<colindex>, 2UL
              2UL<rowindex>, 1UL<colindex>, 2UL
              2UL<rowindex>, 3UL<colindex>, 1UL
              3UL<rowindex>, 2UL<colindex>, 1UL
              0UL<rowindex>, 4UL<colindex>, 3UL
              4UL<rowindex>, 0UL<colindex>, 3UL
              1UL<rowindex>, 5UL<colindex>, 4UL
              5UL<rowindex>, 1UL<colindex>, 4UL
              2UL<rowindex>, 6UL<colindex>, 5UL
              6UL<rowindex>, 2UL<colindex>, 5UL
              3UL<rowindex>, 7UL<colindex>, 6UL
              7UL<rowindex>, 3UL<colindex>, 6UL
              4UL<rowindex>, 5UL<colindex>, 2UL
              5UL<rowindex>, 4UL<colindex>, 2UL
              5UL<rowindex>, 6UL<colindex>, 1UL
              6UL<rowindex>, 5UL<colindex>, 1UL
              6UL<rowindex>, 7UL<colindex>, 3UL
              7UL<rowindex>, 6UL<colindex>, 3UL ]
        )
    )

[<Fact>]
let ``Level bfs 8 node grid start 0`` () =
    let n = uint64 grid8graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level grid8graph start

    Assert.Equal(
        Ok
            [ Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 8 node grid start 0`` () =
    let n = uint64 grid8graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent grid8graph start

    Assert.Equal(
        Ok
            [ Some 0UL
              Some 0UL
              Some 1UL
              Some 2UL
              Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Level bfs 8 node grid start 7`` () =
    let n = uint64 grid8graph.ncols
    let start = singleStart n 7UL
    let result = Graph.BFS.bfs_level grid8graph start

    Assert.Equal(
        Ok
            [ Some 4UL
              Some 3UL
              Some 2UL
              Some 1UL
              Some 3UL
              Some 2UL
              Some 1UL
              Some 0UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 8 node grid start 7`` () =
    let n = uint64 grid8graph.ncols
    let start = singleStart n 7UL
    let result = Graph.BFS.bfs_parent grid8graph start

    Assert.Equal(
        Ok
            [ Some 1UL
              Some 2UL
              Some 3UL
              Some 7UL
              Some 5UL
              Some 6UL
              Some 7UL
              Some 7UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Level bfs 8 node grid start 4`` () =
    let n = uint64 grid8graph.ncols
    let start = singleStart n 4UL
    let result = Graph.BFS.bfs_level grid8graph start

    Assert.Equal(
        Ok
            [ Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL
              Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 8 node grid start 4`` () =
    let n = uint64 grid8graph.ncols
    let start = singleStart n 4UL
    let result = Graph.BFS.bfs_parent grid8graph start

    Assert.Equal(
        Ok
            [ Some 4UL
              Some 0UL
              Some 1UL
              Some 2UL
              Some 4UL
              Some 4UL
              Some 5UL
              Some 6UL ],
        Result.map (unsafes n) result
    )

// ============== 10 nodes random ==============

let private random10graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            10UL<nrows>,
            10UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 4UL
              1UL<rowindex>, 0UL<colindex>, 4UL
              0UL<rowindex>, 5UL<colindex>, 2UL
              5UL<rowindex>, 0UL<colindex>, 2UL
              1UL<rowindex>, 2UL<colindex>, 3UL
              2UL<rowindex>, 1UL<colindex>, 3UL
              1UL<rowindex>, 6UL<colindex>, 5UL
              6UL<rowindex>, 1UL<colindex>, 5UL
              2UL<rowindex>, 3UL<colindex>, 1UL
              3UL<rowindex>, 2UL<colindex>, 1UL
              2UL<rowindex>, 7UL<colindex>, 4UL
              7UL<rowindex>, 2UL<colindex>, 4UL
              3UL<rowindex>, 4UL<colindex>, 2UL
              4UL<rowindex>, 3UL<colindex>, 2UL
              3UL<rowindex>, 8UL<colindex>, 6UL
              8UL<rowindex>, 3UL<colindex>, 6UL
              4UL<rowindex>, 9UL<colindex>, 3UL
              9UL<rowindex>, 4UL<colindex>, 3UL
              5UL<rowindex>, 6UL<colindex>, 1UL
              6UL<rowindex>, 5UL<colindex>, 1UL
              6UL<rowindex>, 7UL<colindex>, 2UL
              7UL<rowindex>, 6UL<colindex>, 2UL
              7UL<rowindex>, 8UL<colindex>, 1UL
              8UL<rowindex>, 7UL<colindex>, 1UL
              8UL<rowindex>, 9UL<colindex>, 4UL
              9UL<rowindex>, 8UL<colindex>, 4UL ]
        )
    )

[<Fact>]
let ``Level bfs 10 node random start 0`` () =
    let n = uint64 random10graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level random10graph start

    Assert.Equal(
        Ok
            [ Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL
              Some 5UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 10 node random start 0`` () =
    let n = uint64 random10graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent random10graph start

    Assert.Equal(
        Ok
            [ Some 0UL
              Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Level bfs 10 node random start 9`` () =
    let n = uint64 random10graph.ncols
    let start = singleStart n 9UL
    let result = Graph.BFS.bfs_level random10graph start

    Assert.Equal(
        Ok
            [ Some 5UL
              Some 4UL
              Some 3UL
              Some 2UL
              Some 1UL
              Some 4UL
              Some 3UL
              Some 2UL
              Some 1UL
              Some 0UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 10 node random start 9`` () =
    let n = uint64 random10graph.ncols
    let start = singleStart n 9UL
    let result = Graph.BFS.bfs_parent random10graph start

    Assert.Equal(
        Ok
            [ Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL
              Some 9UL
              Some 6UL
              Some 7UL
              Some 8UL
              Some 9UL
              Some 9UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Level bfs 10 node random start 5`` () =
    let n = uint64 random10graph.ncols
    let start = singleStart n 5UL
    let result = Graph.BFS.bfs_level random10graph start

    Assert.Equal(
        Ok
            [ Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL
              Some 5UL
              Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 10 node random start 5`` () =
    let n = uint64 random10graph.ncols
    let start = singleStart n 5UL
    let result = Graph.BFS.bfs_parent random10graph start

    Assert.Equal(
        Ok
            [ Some 5UL
              Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 5UL
              Some 5UL
              Some 6UL
              Some 7UL
              Some 8UL ],
        Result.map (unsafes n) result
    )

// ============== 12 nodes big ==============

let private big12graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            12UL<nrows>,
            12UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 1UL
              1UL<rowindex>, 0UL<colindex>, 1UL
              1UL<rowindex>, 11UL<colindex>, 1UL
              11UL<rowindex>, 1UL<colindex>, 1UL
              0UL<rowindex>, 11UL<colindex>, 1UL
              11UL<rowindex>, 0UL<colindex>, 1UL
              2UL<rowindex>, 3UL<colindex>, 1UL
              3UL<rowindex>, 2UL<colindex>, 1UL
              3UL<rowindex>, 4UL<colindex>, 1UL
              4UL<rowindex>, 3UL<colindex>, 1UL
              2UL<rowindex>, 4UL<colindex>, 1UL
              4UL<rowindex>, 2UL<colindex>, 1UL
              5UL<rowindex>, 6UL<colindex>, 1UL
              6UL<rowindex>, 5UL<colindex>, 1UL
              6UL<rowindex>, 7UL<colindex>, 1UL
              7UL<rowindex>, 6UL<colindex>, 1UL
              5UL<rowindex>, 7UL<colindex>, 1UL
              7UL<rowindex>, 5UL<colindex>, 1UL
              8UL<rowindex>, 9UL<colindex>, 1UL
              9UL<rowindex>, 8UL<colindex>, 1UL
              9UL<rowindex>, 10UL<colindex>, 1UL
              10UL<rowindex>, 9UL<colindex>, 1UL
              8UL<rowindex>, 10UL<colindex>, 1UL
              10UL<rowindex>, 8UL<colindex>, 1UL
              1UL<rowindex>, 2UL<colindex>, 2UL
              2UL<rowindex>, 1UL<colindex>, 2UL
              11UL<rowindex>, 4UL<colindex>, 2UL
              4UL<rowindex>, 11UL<colindex>, 2UL
              10UL<rowindex>, 5UL<colindex>, 2UL
              5UL<rowindex>, 10UL<colindex>, 2UL
              8UL<rowindex>, 7UL<colindex>, 2UL
              7UL<rowindex>, 8UL<colindex>, 2UL
              10UL<rowindex>, 11UL<colindex>, 3UL
              11UL<rowindex>, 10UL<colindex>, 3UL
              5UL<rowindex>, 4UL<colindex>, 3UL
              4UL<rowindex>, 5UL<colindex>, 3UL ]
        )
    )

[<Fact>]
let ``Level bfs 12 node big start 0`` () =
    let n = uint64 big12graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level big12graph start

    Assert.Equal(
        Ok
            [ Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 2UL
              Some 3UL
              Some 4UL
              Some 4UL
              Some 3UL
              Some 3UL
              Some 2UL
              Some 1UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 12 node big start 0`` () =
    let n = uint64 big12graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent big12graph start

    Assert.Equal(
        Ok
            [ Some 0UL
              Some 0UL
              Some 1UL
              Some 2UL
              Some 11UL
              Some 4UL
              Some 5UL
              Some 5UL
              Some 10UL
              Some 10UL
              Some 11UL
              Some 0UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Level bfs 12 node big start 11`` () =
    let n = uint64 big12graph.ncols
    let start = singleStart n 11UL
    let result = Graph.BFS.bfs_level big12graph start

    Assert.Equal(
        Ok
            [ Some 1UL
              Some 1UL
              Some 2UL
              Some 2UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 3UL
              Some 2UL
              Some 2UL
              Some 1UL
              Some 0UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 12 node big start 11`` () =
    let n = uint64 big12graph.ncols
    let start = singleStart n 11UL
    let result = Graph.BFS.bfs_parent big12graph start

    Assert.Equal(
        Ok
            [ Some 11UL
              Some 11UL
              Some 1UL
              Some 4UL
              Some 11UL
              Some 4UL
              Some 5UL
              Some 5UL
              Some 10UL
              Some 10UL
              Some 11UL
              Some 11UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Level bfs 12 node big start 6`` () =
    let n = uint64 big12graph.ncols
    let start = singleStart n 6UL
    let result = Graph.BFS.bfs_level big12graph start

    Assert.Equal(
        Ok
            [ Some 4UL
              Some 4UL
              Some 3UL
              Some 3UL
              Some 2UL
              Some 1UL
              Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 2UL
              Some 3UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 12 node big start 6`` () =
    let n = uint64 big12graph.ncols
    let start = singleStart n 6UL
    let result = Graph.BFS.bfs_parent big12graph start

    Assert.Equal(
        Ok
            [ Some 11UL
              Some 2UL
              Some 4UL
              Some 4UL
              Some 5UL
              Some 6UL
              Some 6UL
              Some 6UL
              Some 7UL
              Some 8UL
              Some 5UL
              Some 4UL ],
        Result.map (unsafes n) result
    )

// ============== 10 nodes complex line ==============

let private complexLine10graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            10UL<nrows>,
            10UL<ncols>,
            [ 0UL<rowindex>, 1UL<colindex>, 1UL
              1UL<rowindex>, 0UL<colindex>, 1UL
              1UL<rowindex>, 2UL<colindex>, 2UL
              2UL<rowindex>, 1UL<colindex>, 2UL
              2UL<rowindex>, 3UL<colindex>, 1UL
              3UL<rowindex>, 2UL<colindex>, 1UL
              3UL<rowindex>, 4UL<colindex>, 3UL
              4UL<rowindex>, 3UL<colindex>, 3UL
              4UL<rowindex>, 5UL<colindex>, 1UL
              5UL<rowindex>, 4UL<colindex>, 1UL
              5UL<rowindex>, 6UL<colindex>, 1UL
              6UL<rowindex>, 5UL<colindex>, 1UL
              6UL<rowindex>, 7UL<colindex>, 2UL
              7UL<rowindex>, 6UL<colindex>, 2UL
              7UL<rowindex>, 8UL<colindex>, 1UL
              8UL<rowindex>, 7UL<colindex>, 1UL
              8UL<rowindex>, 9UL<colindex>, 1UL
              9UL<rowindex>, 8UL<colindex>, 1UL ]
        )
    )

[<Fact>]
let ``Level bfs 10 node complex line start 0`` () =
    let n = uint64 complexLine10graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level complexLine10graph start

    Assert.Equal(
        Ok
            [ Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL
              Some 5UL
              Some 6UL
              Some 7UL
              Some 8UL
              Some 9UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 10 node complex line start 0`` () =
    let n = uint64 complexLine10graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent complexLine10graph start

    Assert.Equal(
        Ok
            [ Some 0UL
              Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL
              Some 5UL
              Some 6UL
              Some 7UL
              Some 8UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Level bfs 10 node complex line start 9`` () =
    let n = uint64 complexLine10graph.ncols
    let start = singleStart n 9UL
    let result = Graph.BFS.bfs_level complexLine10graph start

    Assert.Equal(
        Ok
            [ Some 9UL
              Some 8UL
              Some 7UL
              Some 6UL
              Some 5UL
              Some 4UL
              Some 3UL
              Some 2UL
              Some 1UL
              Some 0UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 10 node complex line start 9`` () =
    let n = uint64 complexLine10graph.ncols
    let start = singleStart n 9UL
    let result = Graph.BFS.bfs_parent complexLine10graph start

    Assert.Equal(
        Ok
            [ Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL
              Some 5UL
              Some 6UL
              Some 7UL
              Some 8UL
              Some 9UL
              Some 9UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Level bfs 10 node complex line start 5`` () =
    let n = uint64 complexLine10graph.ncols
    let start = singleStart n 5UL
    let result = Graph.BFS.bfs_level complexLine10graph start

    Assert.Equal(
        Ok
            [ Some 5UL
              Some 4UL
              Some 3UL
              Some 2UL
              Some 1UL
              Some 0UL
              Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL ],
        Result.map (unsafes n) result
    )

[<Fact>]
let ``Parent bfs 10 node complex line start 5`` () =
    let n = uint64 complexLine10graph.ncols
    let start = singleStart n 5UL
    let result = Graph.BFS.bfs_parent complexLine10graph start

    Assert.Equal(
        Ok
            [ Some 1UL
              Some 2UL
              Some 3UL
              Some 4UL
              Some 5UL
              Some 5UL
              Some 5UL
              Some 6UL
              Some 7UL
              Some 8UL ],
        Result.map (unsafes n) result
    )
