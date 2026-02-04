module Matrix.Tests

open System
open Xunit

open Matrix
open Common

let printMatrix (matrix: SparseMatrix<_>) =
    printfn "Matrix:"
    printfn "   Rows: %A" matrix.nrows
    printfn "   Columns: %A" matrix.ncols
    printfn "   Nvals: %A" matrix.nvals
    printfn "   Storage:"
    printfn "      vSize: %A" matrix.storage.vSize
    printfn "      hSize: %A" matrix.storage.hSize
    printfn "      Data: %A" matrix.storage.data


(*
N,1,1,N
3,2,2,3
N,N,1,2
N,N,3,N
+
1,1,2,2
1,1,2,2
3,3,N,N
3,3,N,N
=
N,2,3,N
4,3,4,5
N,N,N,N
N,N,N,N
*)
[<Fact>]
let ``Simple Matrix.map2. Square where number of cols and rows are power of two.`` () =
    let m1 =
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

        let store = Storage(4UL<storageVSize>, 4UL<storageHSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 9UL<nvals>, store)

    let m2 =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Leaf(UserValue(Some(1))),
                Matrix.qtree.Leaf(UserValue(Some(2))),
                Matrix.qtree.Leaf(UserValue(Some(3))),
                Matrix.qtree.Leaf(UserValue(None))
            )

        let store = Storage(4UL<storageVSize>, 4UL<storageHSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 12UL<nvals>, store)

    let f x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | _ -> None

    let expected =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(4))),
                    Matrix.qtree.Leaf(UserValue(Some(3)))
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(4))),
                    Matrix.qtree.Leaf(UserValue(Some(5)))
                ),
                Matrix.qtree.Leaf(UserValue(None)),
                Matrix.qtree.Leaf(UserValue(None))
            )

        let store = Storage(4UL<storageVSize>, 4UL<storageHSize>, tree)
        Result.Success(SparseMatrix(4UL<nrows>, 4UL<ncols>, 6UL<nvals>, store))

    let actual = Matrix.map2 m1 m2 f

    let eq = actual = expected

    Assert.True(eq)

(*
N,1,1,D
3,2,2,D
N,N,1,D
D,D,D,D
+
1,1,2,D
1,1,2,D
3,3,N,D
D,D,D,D
=
N,2,3,D
4,3,4,D
N,N,N,D
D,D,D,D
*)
[<Fact>]
let ``Simple Matrix.map2. Square where number of cols and rows are not power of two.`` () =
    let m1 =
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
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(Dummy)
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(Dummy)
                )
            )

        let store = Storage(4UL<storageVSize>, 4UL<storageHSize>, tree)
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 6UL<nvals>, store)

    let m2 =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Leaf(UserValue(Some(1))),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(Dummy)
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(Dummy)
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(Dummy)
                )
            )

        let store = Storage(4UL<storageVSize>, 4UL<storageHSize>, tree)
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 8UL<nvals>, store)

    let f x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | _ -> None

    let expected =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(4))),
                    Matrix.qtree.Leaf(UserValue(Some(3)))
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(UserValue(Some(4))),
                    Matrix.qtree.Leaf(Dummy)
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(Dummy)
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(Dummy),
                    Matrix.qtree.Leaf(Dummy)
                )
            )

        let store = Storage(4UL<storageVSize>, 4UL<storageHSize>, tree)
        Result.Success(SparseMatrix(3UL<nrows>, 3UL<ncols>, 5UL<nvals>, store))

    let actual = Matrix.map2 m1 m2 f

    let eq = actual = expected

    Assert.True(eq)

[<Fact>]
let ``Conversion identity`` () =
    let id = toCoordinateList << fromCoordinateList

    let nrows = 10UL<nrows>
    let ncols = 12UL<ncols>

    let data =
        [ 0UL<rowindex>, 3UL<colindex>, 10
          3UL<rowindex>, 3UL<colindex>, 33
          9UL<rowindex>, 2UL<colindex>, 5
          3UL<rowindex>, 11UL<colindex>, 1 ]

    let coordinates = CoordinateList(nrows, ncols, data)

    let expected = coordinates
    let actual = id coordinates

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple addition`` () =
    let nrows = 10UL<nrows>
    let ncols = 12UL<ncols>

    let d1 =
        [ 0UL<rowindex>, 3UL<colindex>, 4
          9UL<rowindex>, 2UL<colindex>, 5
          3UL<rowindex>, 11UL<colindex>, 2 ]

    let d2 =
        [ 0UL<rowindex>, 3UL<colindex>, 6
          3UL<rowindex>, 3UL<colindex>, 33
          3UL<rowindex>, 11UL<colindex>, -1 ]

    let expected =
        let expectedList =
            [ 0UL<rowindex>, 3UL<colindex>, 10
              3UL<rowindex>, 3UL<colindex>, 33
              9UL<rowindex>, 2UL<colindex>, 5
              3UL<rowindex>, 11UL<colindex>, 1 ]

        CoordinateList(nrows, ncols, expectedList)

    let actual =
        let c1 = CoordinateList(nrows, ncols, d1)
        let c2 = CoordinateList(nrows, ncols, d2)
        let m1 = fromCoordinateList c1
        let m2 = fromCoordinateList c2

        let addition o1 o2 =
            match o1, o2 with
            | Some x, Some y -> Some(x + y)
            | Some x, None
            | None, Some x -> Some x
            | None, None -> None

        let result =
            match map2 m1 m2 addition with
            | Result.Success x -> x
            | _ -> failwith "Unreachable"

        toCoordinateList result

    Assert.Equal(expected, actual)
