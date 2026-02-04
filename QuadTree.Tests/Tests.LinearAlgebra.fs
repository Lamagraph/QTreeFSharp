module LinearAlgebra.Tests

open System
open Xunit

open Matrix
open Vector
open Common

(*
2,2,2,2
*
N,1,1,N
3,2,2,3
N,N,1,2
N,N,3,N
=
6,6,14,10
*)
[<Fact>]
let ``Simple vxm. All sizes are power of two.`` () =
    let m =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(Some(2)))
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(3)))
                ),
                Matrix.qtree.Leaf(UserValue(None)),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(None))
                )
            )

        let store = Matrix.Storage(4UL<storageVSize>, 4UL<storageHSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 9UL<nvals>, store)

    let v =
        let tree = Vector.btree.Leaf(UserValue(Some(2)))

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(4UL<dataLength>, 4UL<nvals>, store)

    let op_add x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | Some(a), _
        | _, Some(a) -> Some(a)
        | _ -> None

    let op_mult x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a * b)
        | _ -> None

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Leaf(UserValue(Some(6))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(14))), Vector.btree.Leaf(UserValue(Some(10))))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        Result.Success(SparseVector(4UL<dataLength>, 4UL<nvals>, store))

    let actual = LinearAlgebra.vxm op_add op_mult v m

    Assert.Equal(expected, actual)

(*
2,2,2,D
*
N,1,1,N
3,2,2,3
N,N,1,2
D,D,D,D
=
6,6,8,10
*)
[<Fact>]
let ``Simple vxm. 3 * (3x4)`` () =
    let m =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(Some(2)))
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(3)))
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(Dummy)
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(Dummy)
                )
            )

        let store = Matrix.Storage(4UL<storageVSize>, 4UL<storageHSize>, tree)
        SparseMatrix(3UL<nrows>, 4UL<ncols>, 8UL<nvals>, store)

    let v =
        let tree =
            Vector.btree.Node(
                Vector.btree.Leaf(UserValue(Some(2))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(2))), Vector.btree.Leaf(Dummy))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(3UL<dataLength>, 3UL<nvals>, store)

    let op_add x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | Some(a), _
        | _, Some(a) -> Some(a)
        | _ -> None

    let op_mult x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a * b)
        | _ -> None

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Leaf(UserValue(Some(6))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(8))), Vector.btree.Leaf(UserValue(Some(10))))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        Result.Success(SparseVector(4UL<dataLength>, 4UL<nvals>, store))

    let actual = LinearAlgebra.vxm op_add op_mult v m

    Assert.Equal(expected, actual)


(*
2,2,2,2
*
N,1,1,D
3,2,2,D
N,N,1,D
N,N,3,D
=
6,6,14,D
*)
[<Fact>]
let ``Simple vxm. 4 * (4x3).`` () =
    let m =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(Some(2)))
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(Dummy)
                ),
                Matrix.qtree.Leaf(UserValue(None)),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(Dummy)
                )
            )

        let store = Matrix.Storage(4UL<storageVSize>, 4UL<storageHSize>, tree)
        SparseMatrix(4UL<nrows>, 3UL<ncols>, 7UL<nvals>, store)

    let v =
        let tree = Vector.btree.Leaf(UserValue(Some(2)))

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(4UL<dataLength>, 4UL<nvals>, store)

    let op_add x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | Some(a), _
        | _, Some(a) -> Some(a)
        | _ -> None

    let op_mult x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a * b)
        | _ -> None

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Leaf(UserValue(Some(6))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(14))), Vector.btree.Leaf(Dummy))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        Result.Success(SparseVector(3UL<dataLength>, 3UL<nvals>, store))

    let actual = LinearAlgebra.vxm op_add op_mult v m

    Assert.Equal(expected, actual)


(*
2,2,2,D
*
N,1,1,N,N,D,D,D
3,2,2,3,1,D,D,D
N,N,1,2,N,D,D,D
D,D,D,D,D,D,D,D
D,D,D,D,D,D,D,D
D,D,D,D,D,D,D,D
D,D,D,D,D,D,D,D
D,D,D,D,D,D,D,D
=
6,6,8,10,2,D,D,D
*)
[<Fact>]
let ``Simple vxm. 3 * (3x5)`` () =
    let m =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Node(
                        Matrix.qtree.Leaf(UserValue(None)),
                        Matrix.qtree.Leaf(UserValue(Some(1))),
                        Matrix.qtree.Leaf(UserValue(Some(3))),
                        Matrix.qtree.Leaf(UserValue(Some(2)))
                    ),
                    Matrix.qtree.Node(
                        Matrix.qtree.Leaf(UserValue(Some(1))),
                        Matrix.qtree.Leaf(UserValue(None)),
                        Matrix.qtree.Leaf(UserValue(Some(2))),
                        Matrix.qtree.Leaf(UserValue(Some(3)))
                    ),
                    Matrix.qtree.Node(
                        Matrix.qtree.Leaf(UserValue(None)),
                        Matrix.qtree.Leaf(UserValue(None)),
                        Matrix.qtree.Leaf(Dummy),
                        Matrix.qtree.Leaf(Dummy)
                    ),
                    Matrix.qtree.Node(
                        Matrix.qtree.Leaf(UserValue(Some(1))),
                        Matrix.qtree.Leaf(UserValue(Some(2))),
                        Matrix.qtree.Leaf(Dummy),
                        Matrix.qtree.Leaf(Dummy)
                    )
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Node(
                        Matrix.qtree.Leaf(UserValue(None)),
                        Matrix.qtree.Leaf(Dummy),
                        Matrix.qtree.Leaf(UserValue(Some(1))),
                        Matrix.qtree.Leaf(Dummy)
                    ),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Node(
                        Matrix.qtree.Leaf(UserValue(None)),
                        Matrix.qtree.Leaf(Dummy),
                        Matrix.qtree.Leaf(Dummy),
                        Matrix.qtree.Leaf(Dummy)
                    ),
                    Matrix.qtree.Leaf(Dummy)
                ),
                Matrix.qtree.Leaf(Dummy),
                Matrix.qtree.Leaf(Dummy)
            )

        let store = Matrix.Storage(8UL<storageVSize>, 8UL<storageHSize>, tree)
        SparseMatrix(3UL<nrows>, 5UL<ncols>, 9UL<nvals>, store)

    let v =
        let tree =
            Vector.btree.Node(
                Vector.btree.Leaf(UserValue(Some(2))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(2))), Vector.btree.Leaf(Dummy))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(3UL<dataLength>, 3UL<nvals>, store)

    let op_add x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | Some(a), _
        | _, Some(a) -> Some(a)
        | _ -> None

    let op_mult x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a * b)
        | _ -> None

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(
                    Vector.btree.Leaf(UserValue(Some(6))),
                    Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(8))), Vector.btree.Leaf(UserValue(Some(10))))
                ),
                Vector.btree.Node(
                    Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(2))), Vector.btree.Leaf(Dummy)),
                    Vector.btree.Leaf(Dummy)
                )
            )

        let store = Vector.Storage(8UL<storageSize>, tree)
        Result.Success(SparseVector(5UL<dataLength>, 5UL<nvals>, store))

    let actual = LinearAlgebra.vxm op_add op_mult v m

    Assert.Equal(expected, actual)
