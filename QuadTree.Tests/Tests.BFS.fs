module Graph.BFS.Tests

open System
open Xunit

open Matrix
open Vector
open Common

(*
1,N,N,N
,
N,1,1,N
3,N,2,3
N,N,N,2
N,N,3,N
=>
0,1,1,2
*)
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

        let store = Matrix.Storage(4UL<storageVSize>, 4UL<storageHSize>, tree)
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
        Result.Success(SparseVector(4UL<dataLength>, 4UL<nvals>, store))

    let actual = Graph.BFS.bfs_level graph startVertices

    Assert.Equal(expected, actual)
