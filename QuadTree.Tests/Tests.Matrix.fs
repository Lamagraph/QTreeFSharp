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
    printfn "      size: %A" matrix.storage.size
    printfn "      Data: %A" matrix.storage.data

let leaf_v v = qtree.Leaf << UserValue <| Some v
let leaf_n () = qtree.Leaf << UserValue <| None
let leaf_d () = qtree.Leaf Dummy

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
                Matrix.qtree.Node(leaf_n (), leaf_v 1, leaf_v 3, leaf_v 2),
                Matrix.qtree.Node(leaf_v 1, leaf_n (), leaf_v 2, leaf_v 3),
                leaf_n (),
                Matrix.qtree.Node(leaf_v 1, leaf_v 2, leaf_v 3, leaf_n ())
            )

        let store = Storage(4UL<storageSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 9UL<nvals>, store)

    let m2 =
        let tree = Matrix.qtree.Node(leaf_v 1, leaf_v 2, leaf_v 3, leaf_n ())

        let store = Storage(4UL<storageSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 12UL<nvals>, store)

    let f x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | _ -> None

    let expected =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(leaf_n (), leaf_v 2, leaf_v 4, leaf_v 3),
                Matrix.qtree.Node(leaf_v 3, leaf_n (), leaf_v 4, leaf_v 5),
                leaf_n (),
                leaf_n ()
            )

        let store = Storage(4UL<storageSize>, tree)
        Result.Success(SparseMatrix(4UL<nrows>, 4UL<ncols>, 6UL<nvals>, store))

    let actual = Matrix.map2 m1 m2 f

    Assert.Equal(expected, actual)

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
                Matrix.qtree.Node(leaf_n (), leaf_v 1, leaf_v 3, leaf_v 2),
                Matrix.qtree.Node(leaf_v 1, leaf_d (), leaf_v 2, leaf_d ()),
                Matrix.qtree.Node(leaf_n (), leaf_n (), leaf_d (), leaf_d ()),
                Matrix.qtree.Node(leaf_v 1, leaf_d (), leaf_d (), leaf_d ())
            )

        let store = Storage(4UL<storageSize>, tree)
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 6UL<nvals>, store)

    let m2 =
        let tree =
            Matrix.qtree.Node(
                leaf_v 1,
                Matrix.qtree.Node(leaf_v 2, leaf_d (), leaf_v 2, leaf_d ()),
                Matrix.qtree.Node(leaf_v 3, leaf_v 3, leaf_d (), leaf_d ()),
                Matrix.qtree.Node(leaf_n (), leaf_d (), leaf_d (), leaf_d ())
            )

        let store = Storage(4UL<storageSize>, tree)
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 8UL<nvals>, store)

    let f x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | _ -> None

    let expected =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(leaf_n (), leaf_v 2, leaf_v 4, leaf_v 3),
                Matrix.qtree.Node(leaf_v 3, leaf_d (), leaf_v 4, leaf_d ()),
                Matrix.qtree.Node(leaf_n (), leaf_n (), leaf_d (), leaf_d ()),
                Matrix.qtree.Node(leaf_n (), leaf_d (), leaf_d (), leaf_d ())
            )

        let store = Storage(4UL<storageSize>, tree)
        Result.Success(SparseMatrix(3UL<nrows>, 3UL<ncols>, 5UL<nvals>, store))

    let actual = Matrix.map2 m1 m2 f

    Assert.Equal(expected, actual)

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
        |> List.sort

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
            |> List.sort

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

[<Fact>]
let ``Condensation of empty`` () =
    let clist = CoordinateList(2UL<nrows>, 3UL<ncols>, [])

    let actual = fromCoordinateList clist

    // 2 * 3 = 5
    // 4 * 4 None and Dummy
    // NN N D
    // NN N D
    // DDDD
    // DDDD
    let tree =
        qtree.Node(leaf_n (), qtree.Node(leaf_n (), leaf_d (), leaf_n (), leaf_d ()), leaf_d (), leaf_d ())

    let expected =
        SparseMatrix(2UL<nrows>, 3UL<ncols>, 0UL<nvals>, Storage(4UL<storageSize>, tree))

    Assert.Equal(expected.storage.data, actual.storage.data)

[<Fact>]
let ``Condensation of sparse`` () =
    let clist =
        CoordinateList(4UL<nrows>, 3UL<ncols>, [ 0UL<rowindex>, 2UL<colindex>, 2; 3UL<rowindex>, 2UL<colindex>, 4 ])

    let actual = fromCoordinateList clist

    // NN2D
    // NNND
    // NNND
    // NN4D

    let tree =
        qtree.Node(
            leaf_n (),
            qtree.Node(leaf_v 2, leaf_d (), leaf_n (), leaf_d ()),
            leaf_n (),
            qtree.Node(leaf_n (), leaf_d (), leaf_v 4, leaf_d ())
        )

    let expected =
        SparseMatrix(4UL<nrows>, 3UL<ncols>, 0UL<nvals>, Storage(4UL<storageSize>, tree))

    Assert.Equal(expected.storage.data, actual.storage.data)

[<Fact>]
let ``fold -> sum`` () =
    // 222D
    // 222D
    // 222D
    // DDDD
    let tree =
        qtree.Node(
            leaf_v 2,
            qtree.Node(leaf_v 2, leaf_d (), leaf_v 2, leaf_d ()),
            qtree.Node(leaf_v 2, leaf_v 2, leaf_d (), leaf_d ()),
            qtree.Node(leaf_v 2, leaf_d (), leaf_d (), leaf_d ())
        )

    let m1 =
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 9UL<nvals>, Matrix.Storage(4UL<storageSize>, tree))

    let expected = 18

    let actual = Option.get <| Matrix.fold op_add None m1

    Assert.Equal(expected, actual)

[<Fact>]
let ``4x4 lower triangle`` () =
    // 2222
    // 2222
    // 2222
    // 2222
    let tree = leaf_v 2

    // 2NNN
    // 22NN
    // 222N
    // 2222
    let tree_expected =
        qtree.Node(
            qtree.Node(leaf_v 2, leaf_n (), leaf_v 2, leaf_v 2),
            leaf_n (),
            leaf_v 2,
            qtree.Node(leaf_v 2, leaf_n (), leaf_v 2, leaf_v 2)
        )

    let m1 =
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 16UL<nvals>, Matrix.Storage(4UL<storageSize>, tree))

    let expected =
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 10UL<nvals>, Matrix.Storage(4UL<storageSize>, tree_expected))

    let actual = getLowerTriangle m1

    Assert.Equal(expected, actual)


[<Fact>]
let ``3x3 lower triangle`` () =
    // 222 D
    // N22 D
    // NN2 D

    // DDD D
    let tree =
        qtree.Node(
            qtree.Node(leaf_v 2, leaf_v 2, leaf_n (), leaf_v 2),
            qtree.Node(leaf_v 2, leaf_d (), leaf_v 2, leaf_d ()),
            qtree.Node(leaf_n (), leaf_n (), leaf_d (), leaf_d ()),
            qtree.Node(leaf_v 2, leaf_d (), leaf_d (), leaf_d ())
        )


    // 2NN D
    // N2N D
    // NN2 D

    // DDD D
    let tree_expected =
        qtree.Node(
            qtree.Node(leaf_v 2, leaf_n (), leaf_n (), leaf_v 2),
            qtree.Node(leaf_n (), leaf_d (), leaf_n (), leaf_d ()),
            qtree.Node(leaf_n (), leaf_n (), leaf_d (), leaf_d ()),
            qtree.Node(leaf_v 2, leaf_d (), leaf_d (), leaf_d ())
        )

    let m1 =
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 6UL<nvals>, Matrix.Storage(4UL<storageSize>, tree))

    let expected =
        SparseMatrix(3UL<nrows>, 3UL<ncols>, 3UL<nvals>, Matrix.Storage(4UL<storageSize>, tree_expected))

    let actual = getLowerTriangle m1

    Assert.Equal(expected, actual)

[<Fact>]
let ``2x3 transposition`` () =
    // 2N2D
    // N2ND
    // DDDD
    // DDDD
    let tree =
        qtree.Node(
            qtree.Node(leaf_v 2, leaf_n (), leaf_n (), leaf_v 2),
            qtree.Node(leaf_v 2, leaf_d (), leaf_n (), leaf_d ()),
            leaf_d (),
            leaf_d ()
        )


    // 2NDD
    // N2DD
    // 2NDD
    // DDDD
    let tree_expected =
        qtree.Node(
            qtree.Node(leaf_v 2, leaf_n (), leaf_n (), leaf_v 2),
            leaf_d (),
            qtree.Node(leaf_v 2, leaf_n (), leaf_d (), leaf_d ()),
            leaf_d ()
        )

    let m1 =
        SparseMatrix(2UL<nrows>, 3UL<ncols>, 3UL<nvals>, Matrix.Storage(4UL<storageSize>, tree))

    let expected =
        SparseMatrix(3UL<nrows>, 2UL<ncols>, 3UL<nvals>, Matrix.Storage(4UL<storageSize>, tree_expected))

    let actual = transpose m1

    Assert.Equal(expected, actual)
