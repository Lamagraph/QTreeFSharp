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

let printMatrixCoordinate (matrix: SparseMatrix<_>) =
    printfn "Matrix:"
    printfn "   Rows: %A" matrix.nrows
    printfn "   Columns: %A" matrix.ncols
    printfn "   Nvals: %A" matrix.nvals
    printfn "   Storage:"
    printfn "      size: %A" matrix.storage.size
    printfn "      Data: %A" (Matrix.toCoordinateList matrix).list


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
        Ok(SparseMatrix(4UL<nrows>, 4UL<ncols>, 6UL<nvals>, store))

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
        Ok(SparseMatrix(3UL<nrows>, 3UL<ncols>, 5UL<nvals>, store))

    let actual = Matrix.map2 m1 m2 f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Matrix.map2i. Square where number of cols and rows are power of two.`` () =
    let m1 =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1)
                  (0UL<rowindex>, 1UL<colindex>, 2)
                  (1UL<rowindex>, 0UL<colindex>, 3)
                  (1UL<rowindex>, 1UL<colindex>, 4) ]
            )
        )

    let m2 =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 10)
                  (0UL<rowindex>, 1UL<colindex>, 20)
                  (1UL<rowindex>, 0UL<colindex>, 30)
                  (1UL<rowindex>, 1UL<colindex>, 40) ]
            )
        )

    let f row col x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b + int row + int col)
        | _ -> None

    let expected =
        Matrix.CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 11)
              (0UL<rowindex>, 1UL<colindex>, 23)
              (1UL<rowindex>, 0UL<colindex>, 34)
              (1UL<rowindex>, 1UL<colindex>, 46) ]
        )
        |> Matrix.fromCoordinateList
        |> Ok

    let actual = Matrix.map2i m1 m2 f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Matrix.map2i. Square where number of cols and rows are not power of two.`` () =
    let m1 =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                3UL<nrows>,
                3UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1)
                  (0UL<rowindex>, 1UL<colindex>, 2)
                  (0UL<rowindex>, 2UL<colindex>, 3)
                  (1UL<rowindex>, 0UL<colindex>, 4)
                  (1UL<rowindex>, 1UL<colindex>, 5)
                  (1UL<rowindex>, 2UL<colindex>, 6) ]
            )
        )

    let m2 =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                3UL<nrows>,
                3UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 10)
                  (0UL<rowindex>, 1UL<colindex>, 10)
                  (0UL<rowindex>, 2UL<colindex>, 10)
                  (1UL<rowindex>, 0UL<colindex>, 10)
                  (1UL<rowindex>, 1UL<colindex>, 10)
                  (1UL<rowindex>, 2UL<colindex>, 10) ]
            )
        )

    let f row col x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a * (int row + 1) + b * (int col + 1))
        | _ -> None

    let actual = Matrix.map2i m1 m2 f

    let expected =
        Matrix.CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 11)
              (0UL<rowindex>, 1UL<colindex>, 22)
              (0UL<rowindex>, 2UL<colindex>, 33)
              (1UL<rowindex>, 0UL<colindex>, 18)
              (1UL<rowindex>, 1UL<colindex>, 30)
              (1UL<rowindex>, 2UL<colindex>, 42) ]
        )
        |> Matrix.fromCoordinateList
        |> Ok

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Matrix.map2i. Mixed values.`` () =
    let m1 =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 3) ]
            )
        )

    let m2 =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (1UL<rowindex>, 1UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 30) ]
            )
        )

    let f row col x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | Some(a), None -> Some(int col + a * 2)
        | None, Some(b) -> Some(int row + b * 3)
        | _ -> None

    let actual = Matrix.map2i m1 m2 f

    let expected =
        Matrix.CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 2)
              (1UL<rowindex>, 1UL<colindex>, 31)
              (2UL<rowindex>, 2UL<colindex>, 8)
              (3UL<rowindex>, 3UL<colindex>, 93) ]
        )
        |> Matrix.fromCoordinateList
        |> Ok

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Matrix.mapi. Square where number of cols and rows are power of two.`` () =
    let m =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1)
                  (0UL<rowindex>, 1UL<colindex>, 2)
                  (1UL<rowindex>, 0UL<colindex>, 3)
                  (1UL<rowindex>, 1UL<colindex>, 4) ]
            )
        )

    let f row col x =
        match x with
        | Some(a) -> Some(a + int row + int col)
        | _ -> None

    let actual = Matrix.mapi m f
    let actualCL = Matrix.toCoordinateList actual

    Assert.Equal(4UL<nvals>, actual.nvals)

[<Fact>]
let ``Simple Matrix.mapi. Square where number of cols and rows are not power of two.`` () =
    let m =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                3UL<nrows>,
                3UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1)
                  (0UL<rowindex>, 1UL<colindex>, 2)
                  (0UL<rowindex>, 2UL<colindex>, 3)
                  (1UL<rowindex>, 0UL<colindex>, 4)
                  (1UL<rowindex>, 1UL<colindex>, 5)
                  (1UL<rowindex>, 2UL<colindex>, 6) ]
            )
        )

    let f row col x =
        match x with
        | Some(a) -> Some(a * (int row + 1) * (int col + 1))
        | _ -> None

    let actual = Matrix.mapi m f
    let actualCL = Matrix.toCoordinateList actual

    Assert.Equal(6UL<nvals>, actual.nvals)

[<Fact>]
let ``Simple Matrix.mapi. Multiply row index by value.`` () =
    let m =
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 1)
                  (1UL<rowindex>, 1UL<colindex>, 2)
                  (2UL<rowindex>, 2UL<colindex>, 3)
                  (3UL<rowindex>, 3UL<colindex>, 4) ]
            )
        )

    let f row col x =
        match x with
        | Some(a) -> Some(a * int row)
        | _ -> None

    let actual = Matrix.mapi m f
    let actualCL = Matrix.toCoordinateList actual

    Assert.Equal(4UL<nvals>, actual.nvals)

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
            | Ok x -> x
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

    let actual = Option.get <| Matrix.foldAssociative op_add None m1

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

[<Fact>]
let ``Fold sum`` () =
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

    let m1 =
        SparseMatrix(2UL<nrows>, 3UL<ncols>, 3UL<nvals>, Matrix.Storage(4UL<storageSize>, tree))

    let expected = 6

    let actual = foldAssociative op_add None m1 |> Option.get

    Assert.Equal(expected, actual)

[<Fact>]
let ``fromCoordinateList with out-of-range coordinates throws exception`` () =
    let coo =
        CoordinateList(6UL<nrows>, 6UL<ncols>, [ (9UL<rowindex>, 9UL<colindex>, 13) ])

    Assert.Throws<Exception>(fun () -> fromCoordinateList coo |> ignore)

[<Fact>]
let ``fromCoordinateList with unsorted coordinates works correctly`` () =
    let coo =
        CoordinateList(
            7UL<nrows>,
            7UL<ncols>,
            [ (6UL<rowindex>, 6UL<colindex>, 10)
              (1UL<rowindex>, 2UL<colindex>, 8)
              (1UL<rowindex>, 1UL<colindex>, 100) ]
        )

    let matrix = fromCoordinateList coo
    let result = toCoordinateList matrix

    Assert.Equal(
        CoordinateList(
            7UL<nrows>,
            7UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 100)
              (1UL<rowindex>, 2UL<colindex>, 8)
              (6UL<rowindex>, 6UL<colindex>, 10) ]
        ),
        result
    )

[<Fact>]
let ``fromCoordinateList with duplicate indices returns the last of them`` () =
    let coo =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 33); (1UL<rowindex>, 1UL<colindex>, 100) ]
        )

    let matrix = fromCoordinateList coo
    let result = toCoordinateList matrix
    Assert.Equal(CoordinateList(3UL<nrows>, 3UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 100) ]), result)

[<Fact>]
let ``fromCoordinateList with zero size throws Exception`` () =
    let coo =
        CoordinateList(
            0UL<nrows>,
            0UL<ncols>,
            [ (33UL<rowindex>, 33UL<colindex>, 33); (39UL<rowindex>, 39UL<colindex>, 1) ]
        )

    Assert.Throws<Exception>(fun () -> fromCoordinateList coo |> ignore)

[<Fact>]
let ``map works on square matrix`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (1UL<rowindex>, 1UL<colindex>, 22); (2UL<rowindex>, 3UL<colindex>, 37) ]
            )
        )

    let result =
        map matrix (fun (x: int option) ->
            match x with
            | Some(v) -> Some(v + 9)
            | None -> None)

    let coo = toCoordinateList result

    Assert.Equal(
        CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 31); (2UL<rowindex>, 3UL<colindex>, 46) ]
        ),
        coo
    )

[<Fact>]
let ``map works on rectangular matrix`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                6UL<ncols>,
                [ (1UL<rowindex>, 1UL<colindex>, 21); (4UL<rowindex>, 3UL<colindex>, 36) ]
            )
        )

    let result =
        map matrix (fun (x: int option) ->
            match x with
            | Some(v) -> Some(v / 3)
            | None -> None)

    let coo = toCoordinateList result

    Assert.Equal(
        CoordinateList(
            5UL<nrows>,
            6UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 7); (4UL<rowindex>, 3UL<colindex>, 12) ]
        ),
        coo
    )

[<Fact>]
let ``map on empty matrix returns empty matrix`` () =
    let matrix = fromCoordinateList (CoordinateList(0UL<nrows>, 0UL<ncols>, []))

    let result =
        map matrix (fun (x: int option) ->
            match x with
            | Some(v) -> Some(v * 5)
            | None -> None)

    let coo = toCoordinateList result
    Assert.Equal(CoordinateList(0UL<nrows>, 0UL<ncols>, []), coo)

[<Fact>]
let ``map with function that turns all the elements into zeros`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                7UL<ncols>,
                [ (1UL<rowindex>, 1UL<colindex>, 5)
                  (4UL<rowindex>, 1UL<colindex>, 17)
                  (4UL<rowindex>, 6UL<colindex>, 33) ]
            )
        )

    let result = map matrix (fun _ -> Some(0))
    let coo = toCoordinateList result

    let expectedData =
        [ for i in 0UL .. 4UL do
              for j in 0UL .. 6UL -> (i * 1UL<rowindex>, j * 1UL<colindex>, 0) ]

    let expected = CoordinateList(5UL<nrows>, 7UL<ncols>, expectedData)
    Assert.Equal(expected, coo)

[<Fact>]
let ``map with function that turns all the elements to None`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                7UL<ncols>,
                [ (1UL<rowindex>, 1UL<colindex>, 5)
                  (4UL<rowindex>, 1UL<colindex>, 17)
                  (4UL<rowindex>, 6UL<colindex>, 33) ]
            )
        )

    let result = map matrix (fun _ -> None)
    let coo = toCoordinateList result
    Assert.Equal(CoordinateList(5UL<nrows>, 7UL<ncols>, []), coo)

[<Fact>]
let ``map can change type from int to string`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                3UL<nrows>,
                5UL<ncols>,
                [ (2UL<rowindex>, 1UL<colindex>, 17); (2UL<rowindex>, 4UL<colindex>, 33) ]
            )
        )

    let result =
        map matrix (fun (x: int option) ->
            match x with
            | Some v -> Some(sprintf "str %d" v)
            | None -> None)

    let coo = toCoordinateList result

    Assert.Equal(
        CoordinateList(
            3UL<nrows>,
            5UL<ncols>,
            [ (2UL<rowindex>, 1UL<colindex>, "str 17")
              (2UL<rowindex>, 4UL<colindex>, "str 33") ]
        ),
        coo
    )

[<Fact>]
let ``mapi works with row index and col index on square matrix`` () =
    let coo =
        CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 12); (2UL<rowindex>, 3UL<colindex>, 13) ]
        )

    let matrix = fromCoordinateList coo

    let result =
        mapi matrix (fun (i: uint64<rowindex>) (j: uint64<colindex>) (x: int option) ->
            match x with
            | Some v -> Some(v * int (i) + int (j))
            | None -> None)

    let actual = toCoordinateList result

    Assert.Equal(
        CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [ (1UL<rowindex>, 1UL<colindex>, 13); (2UL<rowindex>, 3UL<colindex>, 29) ]
        ),
        actual
    )

[<Fact>]
let ``mapi works with row index and col index on rectangular matrix`` () =
    let coo =
        CoordinateList(
            3UL<nrows>,
            5UL<ncols>,
            [ (1UL<rowindex>, 2UL<colindex>, 15); (2UL<rowindex>, 2UL<colindex>, 13) ]
        )

    let matrix = fromCoordinateList coo

    let result =
        mapi matrix (fun (i: uint64<rowindex>) (j: uint64<colindex>) (x: int option) ->
            match x with
            | Some v -> Some(v * int (j) + int (i))
            | None -> None)

    let actual = toCoordinateList result

    Assert.Equal(
        CoordinateList(
            3UL<nrows>,
            5UL<ncols>,
            [ (1UL<rowindex>, 2UL<colindex>, 31); (2UL<rowindex>, 2UL<colindex>, 28) ]
        ),
        actual
    )

[<Fact>]
let ``mapi on empty matrix returns empty matrix`` () =
    let matrix = fromCoordinateList (CoordinateList(0UL<nrows>, 0UL<ncols>, []))

    let result =
        mapi matrix (fun (i: uint64<rowindex>) (j: uint64<colindex>) (x: int option) ->
            match x with
            | Some v -> Some((v + int (1)) * 3 + 2 * int (j))
            | None -> None)

    let actual = toCoordinateList result
    Assert.Equal(CoordinateList(0UL<nrows>, 0UL<ncols>, []), actual)

[<Fact>]
let ``mapi with special function returns empty matrix`` () =
    let coo =
        CoordinateList(
            5UL<nrows>,
            7UL<ncols>,
            [ (1UL<rowindex>, 2UL<colindex>, 15); (4UL<rowindex>, 5UL<colindex>, 13) ]
        )

    let matrix = fromCoordinateList coo

    let result =
        mapi matrix (fun (i: uint64<rowindex>) (j: uint64<colindex>) (x: int option) ->
            match x with
            | Some v ->
                match (int (i) + int (j)) % 2 with
                | 0 -> Some(v * 2)
                | _ -> None
            | None -> None)

    let actual = toCoordinateList result
    Assert.Equal(CoordinateList(5UL<nrows>, 7UL<ncols>, []), actual)

[<Fact>]
let ``mapi works with function does not depend on the indexes`` () =
    let coo =
        CoordinateList(
            5UL<nrows>,
            7UL<ncols>,
            [ (1UL<rowindex>, 2UL<colindex>, 15); (4UL<rowindex>, 5UL<colindex>, 13) ]
        )

    let matrix = fromCoordinateList coo

    let result =
        mapi matrix (fun (i: uint64<rowindex>) (j: uint64<colindex>) (x: int option) ->
            match x with
            | Some v -> Some(v * 2)
            | None -> None)

    let actual = toCoordinateList result

    Assert.Equal(
        CoordinateList(
            5UL<nrows>,
            7UL<ncols>,
            [ (1UL<rowindex>, 2UL<colindex>, 30); (4UL<rowindex>, 5UL<colindex>, 26) ]
        ),
        actual
    )

[<Fact>]
let ``slice returns error when row start is negative`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                6UL<ncols>,
                [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
            )
        )

    match slice matrix -1 4 2 3 with
    | Result.Ok _ -> Assert.Fail("Expected Error")
    | Result.Error msg -> Assert.Equal("Start row should be >= 0", msg)

[<Fact>]
let ``slice returns error when row end is negative`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                6UL<ncols>,
                [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
            )
        )

    match slice matrix 1 -4 2 3 with
    | Result.Ok _ -> Assert.Fail("Expected Error")
    | Result.Error msg -> Assert.Equal("End row should be >= 0", msg)

[<Fact>]
let ``slice returns error when col start is negative`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                6UL<ncols>,
                [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
            )
        )

    match slice matrix 1 4 -2 3 with
    | Result.Ok _ -> Assert.Fail("Expected Error")
    | Result.Error msg -> Assert.Equal("Start column should be >= 0", msg)

[<Fact>]
let ``slice returns error when col end is negative`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                6UL<ncols>,
                [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
            )
        )

    match slice matrix 1 4 2 -3 with
    | Result.Ok _ -> Assert.Fail("Expected Error")
    | Result.Error msg -> Assert.Equal("End column should be >= 0", msg)

[<Fact>]
let ``slice returns error when row start is out of range`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                6UL<ncols>,
                [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
            )
        )

    match slice matrix 6 4 2 3 with
    | Result.Ok _ -> Assert.Fail("Expected Error")
    | Result.Error msg -> Assert.Equal("Start row is out of matrix length", msg)

[<Fact>]
let ``slice returns error when row end is out of range`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                6UL<ncols>,
                [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
            )
        )

    match slice matrix 1 10 2 3 with
    | Result.Ok _ -> Assert.Fail("Expected Error")
    | Result.Error msg -> Assert.Equal("End row is out of matrix length", msg)

[<Fact>]
let ``slice returns error when col start is out of range`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                6UL<ncols>,
                [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
            )
        )

    match slice matrix 1 4 10 3 with
    | Result.Ok _ -> Assert.Fail("Expected Error")
    | Result.Error msg -> Assert.Equal("Start column is out of matrix length", msg)

[<Fact>]
let ``slice returns error when col end is out of range`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                6UL<ncols>,
                [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
            )
        )

    match slice matrix 1 2 2 10 with
    | Result.Ok _ -> Assert.Fail("Expected Error")
    | Result.Error msg -> Assert.Equal("End column is out of matrix length", msg)

[<Fact>]
let ``slice returns error when row end is less than row start`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                6UL<ncols>,
                [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
            )
        )

    match slice matrix 2 1 2 3 with
    | Result.Ok _ -> Assert.Fail("Expected Error")
    | Result.Error msg -> Assert.Equal("Start row should be <= end row", msg)

[<Fact>]
let ``slice returns error when col end is less than col start`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                6UL<ncols>,
                [ (2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17) ]
            )
        )

    match slice matrix 1 2 3 2 with
    | Result.Ok _ -> Assert.Fail("Expected Error")
    | Result.Error msg -> Assert.Equal("Start column should be <= end column", msg)

[<Fact>]
let ``slice returns correct square submatrix`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                7UL<nrows>,
                7UL<ncols>,
                [ (2UL<rowindex>, 2UL<colindex>, 33); (5UL<rowindex>, 5UL<colindex>, 28) ]
            )
        )

    match slice matrix 1 3 1 3 with
    | Result.Ok result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(3UL<nrows>, 3UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 33) ]), coo)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns correct rectangular submatrix`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                5UL<nrows>,
                9UL<ncols>,
                [ (3UL<rowindex>, 7UL<colindex>, 33); (1UL<rowindex>, 2UL<colindex>, 28) ]
            )
        )

    match slice matrix 2 4 5 8 with
    | Result.Ok result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(3UL<nrows>, 4UL<ncols>, [ (1UL<rowindex>, 2UL<colindex>, 33) ]), coo)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns empty matrix`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                7UL<nrows>,
                9UL<ncols>,
                [ (3UL<rowindex>, 3UL<colindex>, 33); (1UL<rowindex>, 2UL<colindex>, 28) ]
            )
        )

    match slice matrix 4 6 5 8 with
    | Result.Ok result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(3UL<nrows>, 4UL<ncols>, []), coo)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns single submatrix`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                7UL<nrows>,
                9UL<ncols>,
                [ (3UL<rowindex>, 3UL<colindex>, 33); (1UL<rowindex>, 2UL<colindex>, 28) ]
            )
        )

    match slice matrix 1 1 2 2 with
    | Result.Ok result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 28) ]), coo)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns correct submatrix equals to matrix`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                7UL<nrows>,
                9UL<ncols>,
                [ (3UL<rowindex>, 3UL<colindex>, 33); (1UL<rowindex>, 2UL<colindex>, 28) ]
            )
        )

    match slice matrix 0 6 0 8 with
    | Result.Ok result ->
        let coo = toCoordinateList result

        Assert.Equal(
            CoordinateList(
                7UL<nrows>,
                9UL<ncols>,
                [ (1UL<rowindex>, 2UL<colindex>, 28); (3UL<rowindex>, 3UL<colindex>, 33) ]
            ),
            coo
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns correct submatrix when row start of submatrix equals to row start of matrix`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                7UL<nrows>,
                7UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 10)
                  (3UL<rowindex>, 3UL<colindex>, 33)
                  (6UL<rowindex>, 6UL<colindex>, 6) ]
            )
        )

    match slice matrix 0 4 0 4 with
    | Result.Ok result ->
        let coo = toCoordinateList result

        Assert.Equal(
            CoordinateList(
                5UL<nrows>,
                5UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 33) ]
            ),
            coo
        )
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns correct submatrix when row end of submatrix equals to row end of matrix`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                7UL<nrows>,
                7UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 10)
                  (3UL<rowindex>, 3UL<colindex>, 33)
                  (6UL<rowindex>, 6UL<colindex>, 6) ]
            )
        )

    match slice matrix 2 6 2 4 with
    | Result.Ok result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(5UL<nrows>, 3UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 33) ]), coo)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns correct submatrix when col start of submatrix equals to col start of matrix`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                7UL<nrows>,
                7UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 10)
                  (3UL<rowindex>, 3UL<colindex>, 33)
                  (6UL<rowindex>, 6UL<colindex>, 6) ]
            )
        )

    match slice matrix 2 5 0 6 with
    | Result.Ok result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(4UL<nrows>, 7UL<ncols>, [ (1UL<rowindex>, 3UL<colindex>, 33) ]), coo)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns correct submatrix when col end of submatrix equals to col end of matrix`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                7UL<nrows>,
                7UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 10)
                  (3UL<rowindex>, 3UL<colindex>, 33)
                  (6UL<rowindex>, 6UL<colindex>, 6) ]
            )
        )

    match slice matrix 2 4 2 6 with
    | Result.Ok result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(3UL<nrows>, 5UL<ncols>, [ (1UL<rowindex>, 1UL<colindex>, 33) ]), coo)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns single column`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                10UL<nrows>,
                10UL<ncols>,
                [ (1UL<rowindex>, 3UL<colindex>, 1)
                  (2UL<rowindex>, 2UL<colindex>, 2)
                  (5UL<rowindex>, 7UL<colindex>, 3) ]
            )
        )

    match slice matrix 1 6 2 2 with
    | Result.Ok result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(6UL<nrows>, 1UL<ncols>, [ (1UL<rowindex>, 0UL<colindex>, 2) ]), coo)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``slice returns single row`` () =
    let matrix =
        fromCoordinateList (
            CoordinateList(
                10UL<nrows>,
                10UL<ncols>,
                [ (1UL<rowindex>, 3UL<colindex>, 1)
                  (2UL<rowindex>, 2UL<colindex>, 2)
                  (5UL<rowindex>, 7UL<colindex>, 3) ]
            )
        )

    match slice matrix 5 5 3 9 with
    | Result.Ok result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(1UL<nrows>, 7UL<ncols>, [ (0UL<rowindex>, 4UL<colindex>, 3) ]), coo)
    | Result.Error msg -> Assert.Fail(msg)

[<Fact>]
let ``let reduceRows sum on square power of two matrix`` () =
    let coo =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 12)
              (0UL<rowindex>, 1UL<colindex>, 15)
              (1UL<rowindex>, 0UL<colindex>, 17)
              (1UL<rowindex>, 1UL<colindex>, 3) ]
        )

    let matrix = fromCoordinateList coo

    let add x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a + b)

    let result = reduceRows add matrix
    let vectorCoordinates = Vector.toCoordinateList result

    let expected =
        Vector.CoordinateList(2UL<Vector.dataLength>, [ (0UL<Vector.index>, 27); (1UL<Vector.index>, 20) ])

    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``let reduceRows sum on square power of two matrix with empty row`` () =
    let coo =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (1UL<rowindex>, 0UL<colindex>, 17); (1UL<rowindex>, 1UL<colindex>, 3) ]
        )

    let matrix = fromCoordinateList coo

    let add x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a + b)

    let result = reduceRows add matrix
    let vectorCoordinates = Vector.toCoordinateList result

    let expected =
        Vector.CoordinateList(2UL<Vector.dataLength>, [ (1UL<Vector.index>, 20) ])

    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``let reduceRows sum on square not power of two matrix`` () =
    let coo =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 7)
              (0UL<rowindex>, 2UL<colindex>, 9)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (1UL<rowindex>, 2UL<colindex>, 13)
              (2UL<rowindex>, 0UL<colindex>, 15)
              (2UL<rowindex>, 1UL<colindex>, 17) ]
        )

    let matrix = fromCoordinateList coo

    let add x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a + b)

    let result = reduceRows add matrix
    let vectorCoordinates = Vector.toCoordinateList result

    let expected =
        Vector.CoordinateList(
            3UL<Vector.dataLength>,
            [ (0UL<Vector.index>, 21); (1UL<Vector.index>, 24); (2UL<Vector.index>, 32) ]
        )

    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``let reduceRows mul on square not power of two matrix`` () =
    let coo =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 7)
              (0UL<rowindex>, 2UL<colindex>, 9)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (1UL<rowindex>, 2UL<colindex>, 13)
              (2UL<rowindex>, 0UL<colindex>, 15)
              (2UL<rowindex>, 1UL<colindex>, 17) ]
        )

    let matrix = fromCoordinateList coo

    let mul x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a * b)

    let result = reduceRows mul matrix
    let vectorCoordinates = Vector.toCoordinateList result

    let expected =
        Vector.CoordinateList(
            3UL<Vector.dataLength>,
            [ (0UL<Vector.index>, 315); (1UL<Vector.index>, 143); (2UL<Vector.index>, 255) ]
        )

    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``let reduceRows sum on rectangular matrix`` () =
    let coo =
        CoordinateList(
            2UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 7)
              (0UL<rowindex>, 2UL<colindex>, 9)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (1UL<rowindex>, 2UL<colindex>, 13) ]
        )

    let matrix = fromCoordinateList coo

    let sum x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a + b)

    let result = reduceRows sum matrix
    let vectorCoordinates = Vector.toCoordinateList result

    let expected =
        Vector.CoordinateList(2UL<Vector.dataLength>, [ (0UL<Vector.index>, 21); (1UL<Vector.index>, 24) ])

    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``let reduceRows sum on empty matrix`` () =
    let coo = CoordinateList(2UL<nrows>, 3UL<ncols>, [])
    let matrix = fromCoordinateList coo

    let sum x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a + b)

    let result = reduceRows sum matrix
    let vectorCoordinates = Vector.toCoordinateList result
    let expected = Vector.CoordinateList(2UL<Vector.dataLength>, [])
    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``let reduceRows mul on single matrix`` () =
    let coo =
        CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 33) ])

    let matrix = fromCoordinateList coo

    let mul x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a * b)

    let result = reduceRows mul matrix
    let vectorCoordinates = Vector.toCoordinateList result

    let expected =
        Vector.CoordinateList(1UL<Vector.dataLength>, [ (0UL<Vector.index>, 33) ])

    Assert.Equal(expected, vectorCoordinates)


[<Fact>]
let ``let reduceCols sum on square power of two matrix`` () =
    let coo =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 12)
              (0UL<rowindex>, 1UL<colindex>, 15)
              (1UL<rowindex>, 0UL<colindex>, 17)
              (1UL<rowindex>, 1UL<colindex>, 3) ]
        )

    let matrix = fromCoordinateList coo

    let add x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a + b)

    let result = reduceCols add matrix
    let vectorCoordinates = Vector.toCoordinateList result

    let expected =
        Vector.CoordinateList(2UL<Vector.dataLength>, [ (0UL<Vector.index>, 29); (1UL<Vector.index>, 18) ])

    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``let reduceCols sum on square power of two matrix with empty col`` () =
    let coo =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 17); (1UL<rowindex>, 0UL<colindex>, 3) ]
        )

    let matrix = fromCoordinateList coo

    let add x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a + b)

    let result = reduceCols add matrix
    let vectorCoordinates = Vector.toCoordinateList result

    let expected =
        Vector.CoordinateList(2UL<Vector.dataLength>, [ (0UL<Vector.index>, 20) ])

    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``let reduceCols sum on square not power of two matrix`` () =
    let coo =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 7)
              (0UL<rowindex>, 2UL<colindex>, 9)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (1UL<rowindex>, 2UL<colindex>, 13)
              (2UL<rowindex>, 0UL<colindex>, 15)
              (2UL<rowindex>, 1UL<colindex>, 17) ]
        )

    let matrix = fromCoordinateList coo

    let add x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a + b)

    let result = reduceCols add matrix
    let vectorCoordinates = Vector.toCoordinateList result

    let expected =
        Vector.CoordinateList(
            3UL<Vector.dataLength>,
            [ (0UL<Vector.index>, 20); (1UL<Vector.index>, 35); (2UL<Vector.index>, 22) ]
        )

    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``let reduceCols mul on square not power of two matrix`` () =
    let coo =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 7)
              (0UL<rowindex>, 2UL<colindex>, 9)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (1UL<rowindex>, 2UL<colindex>, 13)
              (2UL<rowindex>, 0UL<colindex>, 15)
              (2UL<rowindex>, 1UL<colindex>, 17) ]
        )

    let matrix = fromCoordinateList coo

    let mul x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a * b)

    let result = reduceCols mul matrix
    let vectorCoordinates = Vector.toCoordinateList result

    let expected =
        Vector.CoordinateList(
            3UL<Vector.dataLength>,
            [ (0UL<Vector.index>, 75); (1UL<Vector.index>, 1309); (2UL<Vector.index>, 117) ]
        )

    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``let reduceCols sum on rectangular matrix`` () =
    let coo =
        CoordinateList(
            2UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 7)
              (0UL<rowindex>, 2UL<colindex>, 9)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (1UL<rowindex>, 2UL<colindex>, 13) ]
        )

    let matrix = fromCoordinateList coo

    let sum x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a + b)

    let result = reduceCols sum matrix
    let vectorCoordinates = Vector.toCoordinateList result

    let expected =
        Vector.CoordinateList(
            3UL<Vector.dataLength>,
            [ (0UL<Vector.index>, 5); (1UL<Vector.index>, 18); (2UL<Vector.index>, 22) ]
        )

    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``let reduceCols sum on empty matrix`` () =
    let coo = CoordinateList(2UL<nrows>, 3UL<ncols>, [])
    let matrix = fromCoordinateList coo

    let sum x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a + b)

    let result = reduceCols sum matrix
    let vectorCoordinates = Vector.toCoordinateList result
    let expected = Vector.CoordinateList(3UL<Vector.dataLength>, [])
    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``let reduceCols mul on single matrix`` () =
    let coo =
        CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 33) ])

    let matrix = fromCoordinateList coo

    let mul x y =
        match x, y with
        | None, None -> None
        | None, Some(b) -> Some(b)
        | Some(a), None -> Some(a)
        | Some(a), Some(b) -> Some(a * b)

    let result = reduceCols mul matrix
    let vectorCoordinates = Vector.toCoordinateList result

    let expected =
        Vector.CoordinateList(1UL<Vector.dataLength>, [ (0UL<Vector.index>, 33) ])

    Assert.Equal(expected, vectorCoordinates)

[<Fact>]
let ``kronecker product with square power of 2 x square power of two matrixes`` () =
    let cooA =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let A = fromCoordinateList cooA

    let cooB =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 6)
              (1UL<rowindex>, 0UL<colindex>, 7)
              (1UL<rowindex>, 1UL<colindex>, 8) ]
        )

    let B = fromCoordinateList cooB

    let multiplyOp a b = Some(a * b)

    match kroneckerProduct A B multiplyOp with
    | Error msg -> Assert.True(false, msg)
    | Ok result ->
        let coo = toCoordinateList result

        let expected =
            CoordinateList(
                4UL<nrows>,
                4UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 5)
                  (0UL<rowindex>, 1UL<colindex>, 6)
                  (0UL<rowindex>, 2UL<colindex>, 10)
                  (0UL<rowindex>, 3UL<colindex>, 12)
                  (1UL<rowindex>, 0UL<colindex>, 7)
                  (1UL<rowindex>, 1UL<colindex>, 8)
                  (1UL<rowindex>, 2UL<colindex>, 14)
                  (1UL<rowindex>, 3UL<colindex>, 16)
                  (2UL<rowindex>, 0UL<colindex>, 15)
                  (2UL<rowindex>, 1UL<colindex>, 18)
                  (2UL<rowindex>, 2UL<colindex>, 20)
                  (2UL<rowindex>, 3UL<colindex>, 24)
                  (3UL<rowindex>, 0UL<colindex>, 21)
                  (3UL<rowindex>, 1UL<colindex>, 24)
                  (3UL<rowindex>, 2UL<colindex>, 28)
                  (3UL<rowindex>, 3UL<colindex>, 32) ]
            )

        Assert.Equal(expected, coo)

[<Fact>]
let ``kronecker product with square not power of 2 x square not power of two matrixes`` () =
    let cooA =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (0UL<rowindex>, 2UL<colindex>, 3)
              (1UL<rowindex>, 0UL<colindex>, 4)
              (1UL<rowindex>, 1UL<colindex>, 5)
              (1UL<rowindex>, 2UL<colindex>, 6)
              (2UL<rowindex>, 0UL<colindex>, 7)
              (2UL<rowindex>, 1UL<colindex>, 8)
              (2UL<rowindex>, 2UL<colindex>, 9) ]
        )

    let A = fromCoordinateList cooA

    let cooB =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 10)
              (0UL<rowindex>, 1UL<colindex>, 11)
              (0UL<rowindex>, 2UL<colindex>, 12)
              (1UL<rowindex>, 0UL<colindex>, 13)
              (1UL<rowindex>, 1UL<colindex>, 14)
              (1UL<rowindex>, 2UL<colindex>, 15)
              (2UL<rowindex>, 0UL<colindex>, 16)
              (2UL<rowindex>, 1UL<colindex>, 17)
              (2UL<rowindex>, 2UL<colindex>, 18) ]
        )

    let B = fromCoordinateList cooB

    let multiplyOp a b = Some(a * b)

    match kroneckerProduct A B multiplyOp with
    | Error msg -> Assert.True(false, msg)
    | Ok result ->
        let coo = toCoordinateList result

        let expected =
            CoordinateList(
                9UL<nrows>,
                9UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 10)
                  (0UL<rowindex>, 1UL<colindex>, 11)
                  (0UL<rowindex>, 2UL<colindex>, 12)
                  (0UL<rowindex>, 3UL<colindex>, 20)
                  (0UL<rowindex>, 4UL<colindex>, 22)
                  (0UL<rowindex>, 5UL<colindex>, 24)
                  (0UL<rowindex>, 6UL<colindex>, 30)
                  (0UL<rowindex>, 7UL<colindex>, 33)
                  (0UL<rowindex>, 8UL<colindex>, 36)
                  (1UL<rowindex>, 0UL<colindex>, 13)
                  (1UL<rowindex>, 1UL<colindex>, 14)
                  (1UL<rowindex>, 2UL<colindex>, 15)
                  (1UL<rowindex>, 3UL<colindex>, 26)
                  (1UL<rowindex>, 4UL<colindex>, 28)
                  (1UL<rowindex>, 5UL<colindex>, 30)
                  (1UL<rowindex>, 6UL<colindex>, 39)
                  (1UL<rowindex>, 7UL<colindex>, 42)
                  (1UL<rowindex>, 8UL<colindex>, 45)
                  (2UL<rowindex>, 0UL<colindex>, 16)
                  (2UL<rowindex>, 1UL<colindex>, 17)
                  (2UL<rowindex>, 2UL<colindex>, 18)
                  (2UL<rowindex>, 3UL<colindex>, 32)
                  (2UL<rowindex>, 4UL<colindex>, 34)
                  (2UL<rowindex>, 5UL<colindex>, 36)
                  (2UL<rowindex>, 6UL<colindex>, 48)
                  (2UL<rowindex>, 7UL<colindex>, 51)
                  (2UL<rowindex>, 8UL<colindex>, 54)
                  (3UL<rowindex>, 0UL<colindex>, 40)
                  (3UL<rowindex>, 1UL<colindex>, 44)
                  (3UL<rowindex>, 2UL<colindex>, 48)
                  (3UL<rowindex>, 3UL<colindex>, 50)
                  (3UL<rowindex>, 4UL<colindex>, 55)
                  (3UL<rowindex>, 5UL<colindex>, 60)
                  (3UL<rowindex>, 6UL<colindex>, 60)
                  (3UL<rowindex>, 7UL<colindex>, 66)
                  (3UL<rowindex>, 8UL<colindex>, 72)
                  (4UL<rowindex>, 0UL<colindex>, 52)
                  (4UL<rowindex>, 1UL<colindex>, 56)
                  (4UL<rowindex>, 2UL<colindex>, 60)
                  (4UL<rowindex>, 3UL<colindex>, 65)
                  (4UL<rowindex>, 4UL<colindex>, 70)
                  (4UL<rowindex>, 5UL<colindex>, 75)
                  (4UL<rowindex>, 6UL<colindex>, 78)
                  (4UL<rowindex>, 7UL<colindex>, 84)
                  (4UL<rowindex>, 8UL<colindex>, 90)
                  (5UL<rowindex>, 0UL<colindex>, 64)
                  (5UL<rowindex>, 1UL<colindex>, 68)
                  (5UL<rowindex>, 2UL<colindex>, 72)
                  (5UL<rowindex>, 3UL<colindex>, 80)
                  (5UL<rowindex>, 4UL<colindex>, 85)
                  (5UL<rowindex>, 5UL<colindex>, 90)
                  (5UL<rowindex>, 6UL<colindex>, 96)
                  (5UL<rowindex>, 7UL<colindex>, 102)
                  (5UL<rowindex>, 8UL<colindex>, 108)
                  (6UL<rowindex>, 0UL<colindex>, 70)
                  (6UL<rowindex>, 1UL<colindex>, 77)
                  (6UL<rowindex>, 2UL<colindex>, 84)
                  (6UL<rowindex>, 3UL<colindex>, 80)
                  (6UL<rowindex>, 4UL<colindex>, 88)
                  (6UL<rowindex>, 5UL<colindex>, 96)
                  (6UL<rowindex>, 6UL<colindex>, 90)
                  (6UL<rowindex>, 7UL<colindex>, 99)
                  (6UL<rowindex>, 8UL<colindex>, 108)
                  (7UL<rowindex>, 0UL<colindex>, 91)
                  (7UL<rowindex>, 1UL<colindex>, 98)
                  (7UL<rowindex>, 2UL<colindex>, 105)
                  (7UL<rowindex>, 3UL<colindex>, 104)
                  (7UL<rowindex>, 4UL<colindex>, 112)
                  (7UL<rowindex>, 5UL<colindex>, 120)
                  (7UL<rowindex>, 6UL<colindex>, 117)
                  (7UL<rowindex>, 7UL<colindex>, 126)
                  (7UL<rowindex>, 8UL<colindex>, 135)
                  (8UL<rowindex>, 0UL<colindex>, 112)
                  (8UL<rowindex>, 1UL<colindex>, 119)
                  (8UL<rowindex>, 2UL<colindex>, 126)
                  (8UL<rowindex>, 3UL<colindex>, 128)
                  (8UL<rowindex>, 4UL<colindex>, 136)
                  (8UL<rowindex>, 5UL<colindex>, 144)
                  (8UL<rowindex>, 6UL<colindex>, 144)
                  (8UL<rowindex>, 7UL<colindex>, 153)
                  (8UL<rowindex>, 8UL<colindex>, 162) ]
            )

        Assert.Equal(expected, coo)

[<Fact>]
let ``kronecker product with rectangular and square matrixes`` () =
    let cooA =
        CoordinateList(
            2UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 2UL<colindex>, 4) ]
        )

    let A = fromCoordinateList cooA

    let cooB =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 6)
              (1UL<rowindex>, 0UL<colindex>, 7)
              (1UL<rowindex>, 1UL<colindex>, 8) ]
        )

    let B = fromCoordinateList cooB

    let result = kroneckerProduct A B (fun a b -> Some(a * b))

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok res ->
        let coo = toCoordinateList res

        let expectedElements =
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 6)
              (1UL<rowindex>, 0UL<colindex>, 7)
              (1UL<rowindex>, 1UL<colindex>, 8)

              (0UL<rowindex>, 2UL<colindex>, 10)
              (0UL<rowindex>, 3UL<colindex>, 12)
              (1UL<rowindex>, 2UL<colindex>, 14)
              (1UL<rowindex>, 3UL<colindex>, 16)

              (2UL<rowindex>, 0UL<colindex>, 15)
              (2UL<rowindex>, 1UL<colindex>, 18)
              (3UL<rowindex>, 0UL<colindex>, 21)
              (3UL<rowindex>, 1UL<colindex>, 24)

              (2UL<rowindex>, 4UL<colindex>, 20)
              (2UL<rowindex>, 5UL<colindex>, 24)
              (3UL<rowindex>, 4UL<colindex>, 28)
              (3UL<rowindex>, 5UL<colindex>, 32) ]
            |> List.sortBy (fun (r, c, _) -> (r, c))

        let actualElements = coo.list |> List.sortBy (fun (r, c, _) -> (r, c))

        Assert.Equal(4UL<nrows>, coo.nrows)
        Assert.Equal(6UL<ncols>, coo.ncols)
        Assert.Equal<seq<uint64<rowindex> * uint64<colindex> * int>>(expectedElements, actualElements)

[<Fact>]
let ``kronecker product with square and rectangular matrixes`` () =
    let cooA =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let A = fromCoordinateList cooA

    let cooB =
        CoordinateList(
            2UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 6)
              (0UL<rowindex>, 2UL<colindex>, 7)
              (1UL<rowindex>, 0UL<colindex>, 8)
              (1UL<rowindex>, 1UL<colindex>, 9)
              (1UL<rowindex>, 2UL<colindex>, 10) ]
        )

    let B = fromCoordinateList cooB

    let result = kroneckerProduct A B (fun a b -> Some(a * b))

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok res ->
        let coo = toCoordinateList res

        let expectedElements =
            [ (0UL<rowindex>, 0UL<colindex>, 5)
              (0UL<rowindex>, 1UL<colindex>, 6)
              (0UL<rowindex>, 2UL<colindex>, 7)
              (1UL<rowindex>, 0UL<colindex>, 8)
              (1UL<rowindex>, 1UL<colindex>, 9)
              (1UL<rowindex>, 2UL<colindex>, 10)

              (0UL<rowindex>, 3UL<colindex>, 10)
              (0UL<rowindex>, 4UL<colindex>, 12)
              (0UL<rowindex>, 5UL<colindex>, 14)
              (1UL<rowindex>, 3UL<colindex>, 16)
              (1UL<rowindex>, 4UL<colindex>, 18)
              (1UL<rowindex>, 5UL<colindex>, 20)

              (2UL<rowindex>, 0UL<colindex>, 15)
              (2UL<rowindex>, 1UL<colindex>, 18)
              (2UL<rowindex>, 2UL<colindex>, 21)
              (3UL<rowindex>, 0UL<colindex>, 24)
              (3UL<rowindex>, 1UL<colindex>, 27)
              (3UL<rowindex>, 2UL<colindex>, 30)

              (2UL<rowindex>, 3UL<colindex>, 20)
              (2UL<rowindex>, 4UL<colindex>, 24)
              (2UL<rowindex>, 5UL<colindex>, 28)
              (3UL<rowindex>, 3UL<colindex>, 32)
              (3UL<rowindex>, 4UL<colindex>, 36)
              (3UL<rowindex>, 5UL<colindex>, 40) ]
            |> List.sortBy (fun (r, c, _) -> (r, c))

        let actualElements = coo.list |> List.sortBy (fun (r, c, _) -> (r, c))

        Assert.Equal(4UL<nrows>, coo.nrows)
        Assert.Equal(6UL<ncols>, coo.ncols)
        Assert.Equal<seq<uint64<rowindex> * uint64<colindex> * int>>(expectedElements, actualElements)

[<Fact>]
let ``kronecker product of matrix with empty matrix`` () =
    let cooA =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let A = fromCoordinateList cooA

    let emptyMatrix = fromCoordinateList (CoordinateList(2UL<nrows>, 2UL<ncols>, []))

    let result = kroneckerProduct A emptyMatrix (fun a b -> Some(a * b))

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok res ->
        let coo = toCoordinateList res
        let expected = CoordinateList(4UL<nrows>, 4UL<ncols>, [])
        Assert.Equal(expected, coo)

[<Fact>]
let ``kronecker product of empty matrix with matrix`` () =
    let emptyMatrix = fromCoordinateList (CoordinateList(2UL<nrows>, 2UL<ncols>, []))

    let cooB =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let B = fromCoordinateList cooB

    let result = kroneckerProduct emptyMatrix B (fun a b -> Some(a * b))

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok res ->
        let coo = toCoordinateList res
        let expected = CoordinateList(4UL<nrows>, 4UL<ncols>, [])
        Assert.Equal(expected, coo)

[<Fact>]
let ``kronecker product of matrix with zeros`` () =
    let cooA =
        CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 2) ])

    let A = fromCoordinateList cooA

    let cooB =
        CoordinateList(2UL<nrows>, 2UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 0); (1UL<rowindex>, 1UL<colindex>, 3) ])

    let B = fromCoordinateList cooB

    let result = kroneckerProduct A B (fun a b -> Some(a * b))

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok res ->
        let coo = toCoordinateList res

        let expectedElements =
            [ (0UL<rowindex>, 0UL<colindex>, 0); (1UL<rowindex>, 1UL<colindex>, 6) ]
            |> List.sortBy (fun (r, c, _) -> (r, c))

        let actualElements = coo.list |> List.sortBy (fun (r, c, _) -> (r, c))

        Assert.Equal(2UL<nrows>, coo.nrows)
        Assert.Equal(2UL<ncols>, coo.ncols)
        Assert.Equal<seq<uint64<rowindex> * uint64<colindex> * int>>(expectedElements, actualElements)

[<Fact>]
let ``kronecker product resulting entirely in explicit zeros`` () =
    let cooA =
        CoordinateList(2UL<nrows>, 2UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 0); (1UL<rowindex>, 1UL<colindex>, 0) ])

    let A = fromCoordinateList cooA

    let cooB =
        CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 5) ])

    let B = fromCoordinateList cooB

    let result = kroneckerProduct A B (fun a b -> Some(a * b))

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok res ->
        let coo = toCoordinateList res

        let expectedElements =
            [ (0UL<rowindex>, 0UL<colindex>, 0); (1UL<rowindex>, 1UL<colindex>, 0) ]
            |> List.sortBy (fun (r, c, _) -> (r, c))

        let actualElements = coo.list |> List.sortBy (fun (r, c, _) -> (r, c))

        Assert.Equal(2UL<nrows>, coo.nrows)
        Assert.Equal(2UL<ncols>, coo.ncols)
        Assert.Equal<seq<uint64<rowindex> * uint64<colindex> * int>>(expectedElements, actualElements)

[<Fact>]
let ``kronecker product of square matrix with matrix 1x1`` () =
    let cooA =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let A = fromCoordinateList cooA

    let cooB =
        CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 3) ])

    let B = fromCoordinateList cooB

    let result = kroneckerProduct A B (fun a b -> Some(a * b))

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok res ->
        let coo = toCoordinateList res

        let expected =
            CoordinateList(
                2UL<nrows>,
                2UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 3)
                  (0UL<rowindex>, 1UL<colindex>, 6)
                  (1UL<rowindex>, 0UL<colindex>, 9)
                  (1UL<rowindex>, 1UL<colindex>, 12) ]
            )

        Assert.Equal(expected, coo)

[<Fact>]
let ``kronecker product of matrix 1x1 with square matrix`` () =
    let cooA =
        CoordinateList(1UL<nrows>, 1UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 3) ])

    let A = fromCoordinateList cooA

    let cooB =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let B = fromCoordinateList cooB

    let result = kroneckerProduct A B (fun a b -> Some(a * b))

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok res ->
        let coo = toCoordinateList res

        let expected =
            CoordinateList(
                2UL<nrows>,
                2UL<ncols>,
                [ (0UL<rowindex>, 0UL<colindex>, 3)
                  (0UL<rowindex>, 1UL<colindex>, 6)
                  (1UL<rowindex>, 0UL<colindex>, 9)
                  (1UL<rowindex>, 1UL<colindex>, 12) ]
            )

        Assert.Equal(expected, coo)

[<Fact>]
let ``kronecker dimension check`` () =
    let cooA =
        CoordinateList(3UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    let A = fromCoordinateList cooA

    let cooB =
        CoordinateList(2UL<nrows>, 5UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    let B = fromCoordinateList cooB

    let result = kroneckerProduct A B (fun a b -> Some(a * b))

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok res ->
        Assert.Equal(6UL<nrows>, res.nrows)
        Assert.Equal(20UL<ncols>, res.ncols)

[<Fact>]
let ``kronecker product with sparse matrix on dense matrix`` () =
    let cooA =
        CoordinateList(10UL<nrows>, 10UL<ncols>, [ (5UL<rowindex>, 5UL<colindex>, 2) ])

    let A = fromCoordinateList cooA

    let cooB =
        CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (0UL<rowindex>, 2UL<colindex>, 3)
              (1UL<rowindex>, 0UL<colindex>, 4)
              (1UL<rowindex>, 1UL<colindex>, 5)
              (1UL<rowindex>, 2UL<colindex>, 6)
              (2UL<rowindex>, 0UL<colindex>, 7)
              (2UL<rowindex>, 1UL<colindex>, 8)
              (2UL<rowindex>, 2UL<colindex>, 9) ]
        )

    let B = fromCoordinateList cooB

    let result = kroneckerProduct A B (fun a b -> Some(a * b))

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok res ->
        let coo = toCoordinateList res

        let expected =
            CoordinateList(
                30UL<nrows>,
                30UL<ncols>,
                [ (15UL<rowindex>, 15UL<colindex>, 2)
                  (15UL<rowindex>, 16UL<colindex>, 4)
                  (15UL<rowindex>, 17UL<colindex>, 6)
                  (16UL<rowindex>, 15UL<colindex>, 8)
                  (16UL<rowindex>, 16UL<colindex>, 10)
                  (16UL<rowindex>, 17UL<colindex>, 12)
                  (17UL<rowindex>, 15UL<colindex>, 14)
                  (17UL<rowindex>, 16UL<colindex>, 16)
                  (17UL<rowindex>, 17UL<colindex>, 18) ]
            )

        Assert.Equal(expected, coo)

[<Fact>]
let ``kronecker product with filtering (only even results)`` () =
    let cooA =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let A = fromCoordinateList cooA

    let cooB =
        CoordinateList(
            2UL<nrows>,
            2UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3)
              (1UL<rowindex>, 1UL<colindex>, 4) ]
        )

    let B = fromCoordinateList cooB

    let evenOnly a b =
        let prod = a * b
        if prod % 2 = 0 then Some prod else None

    let result = kroneckerProduct A B evenOnly

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok res ->
        let coo = toCoordinateList res
        Assert.True(coo.list |> List.forall (fun (_, _, v) -> v % 2 = 0))
        Assert.True(List.length coo.list < 16)
