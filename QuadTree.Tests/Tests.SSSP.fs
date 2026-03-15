module Graph.SSSP.Tests

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
let ``Simple SSSP.`` () =
    let graph =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(1.0))),
                    Matrix.qtree.Leaf(UserValue(Some(3.0))),
                    Matrix.qtree.Leaf(UserValue(None))
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(1.0))),
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2.0))),
                    Matrix.qtree.Leaf(UserValue(Some(3.0)))
                ),
                Matrix.qtree.Leaf(UserValue(None)),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2.0))),
                    Matrix.qtree.Leaf(UserValue(Some(3.0))),
                    Matrix.qtree.Leaf(UserValue(None))
                )
            )

        let store = Matrix.Storage(4UL<storageSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 9UL<nvals>, store)

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(0.0))), Vector.btree.Leaf(UserValue(Some(1.0)))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1.0))), Vector.btree.Leaf(UserValue(Some(3.0))))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        Result.Success(SparseVector(4UL<dataLength>, 4UL<nvals>, store))

    let actual = Graph.SSSP.sssp graph 0UL

    Assert.Equal(expected, actual)


(*
0 -1-> 1
1 -1-> 2
2 -1-> 3
2 -4-> 4
0 -6-> 3
3 -2-> 4
*)
[<Fact>]
let ``SSSP with recalculation`` () =
    let graph =
        let clist =
            Matrix.CoordinateList(
                5UL<nrows>,
                5UL<ncols>,
                [ 0UL<rowindex>, 1UL<colindex>, 1.0
                  1UL<rowindex>, 2UL<colindex>, 1.0
                  2UL<rowindex>, 3UL<colindex>, 1.0
                  2UL<rowindex>, 4UL<colindex>, 4.0
                  3UL<rowindex>, 4UL<colindex>, 2.0
                  0UL<rowindex>, 3UL<colindex>, 6.0 ]
            )

        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Vector.CoordinateList(
                5UL<dataLength>,
                [ (0UL<index>, 0.0)
                  (1UL<index>, 1.0)
                  (2UL<index>, 2.0)
                  (3UL<index>, 3.0)
                  (4UL<index>, 5.0) ]
            )

        Result.Success(Vector.fromCoordinateList clist)

    let actual = Graph.SSSP.sssp graph 0UL

    Assert.Equal(expected, actual)
