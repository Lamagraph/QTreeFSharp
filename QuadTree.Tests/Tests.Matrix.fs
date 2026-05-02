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
let ``fromCoordinateList with index out of range`` () =
    let coo = CoordinateList (6UL<nrows>, 6UL<ncols>, [(9UL<rowindex>, 9UL<colindex>, 13)])
    let matrix = fromCoordinateList coo
    let result = toCoordinateList matrix
    Assert.Equal(CoordinateList(6UL<nrows>, 6UL<ncols>, []), result)

[<Fact>]
let ``fromCoordinateList with unsorted coordinates works correctly`` () =
    let coo = CoordinateList (7UL<nrows>, 7UL<ncols>, [(6UL<rowindex>, 6UL<colindex>, 10); (1UL<rowindex>, 2UL<colindex>, 8); (1UL<rowindex>, 1UL<colindex>, 100)])
    let matrix = fromCoordinateList coo
    let result = toCoordinateList matrix
    Assert.Equal(CoordinateList (7UL<nrows>, 7UL<ncols>, [(1UL<rowindex>, 1UL<colindex>, 100); (1UL<rowindex>, 2UL<colindex>, 8); (6UL<rowindex>, 6UL<colindex>, 10)]), result)

[<Fact>]
let ``fromCoordinateList with zero length and some values returns empty matrix`` () =
    let coo = CoordinateList (0UL<nrows>, 0UL<ncols>, [(33UL<rowindex>, 33UL<colindex>, 33); (39UL<rowindex>, 39UL<colindex>, 1)])
    let matrix = fromCoordinateList coo
    let result = toCoordinateList matrix
    Assert.Equal(CoordinateList (0UL<nrows>, 0UL<ncols>, []), result)

[<Fact>]
let ``mapi works with row index and col index on square matrix`` () =
    let coo = CoordinateList (4UL<nrows>, 4UL<ncols>, [(1UL<rowindex>, 1UL<colindex>, 12); (2UL<rowindex>, 3UL<colindex>, 13)])
    let matrix = fromCoordinateList coo
    let result = mapi matrix (fun i j (x: int option) ->
        match x with
        | Some v -> Some (v * i + j)
        | None -> None
    )
    let actual = toCoordinateList result
    Assert.Equal(CoordinateList (4UL<nrows>, 4UL<ncols>, [(1UL<rowindex>, 1UL<colindex>, 13); (2UL<rowindex>, 3UL<colindex>, 29)]), actual)

[<Fact>]
let ``mapi works with row index and col index on rectangular matrix`` () =
    let coo = CoordinateList (3UL<nrows>, 5UL<ncols>, [(1UL<rowindex>, 2UL<colindex>, 15); (2UL<rowindex>, 2UL<colindex>, 13)])
    let matrix = fromCoordinateList coo
    let result = mapi matrix (fun i j (x: int option) ->
        match x with
        | Some v -> Some (v * j + i)
        | None -> None
    )
    let actual = toCoordinateList result
    Assert.Equal(CoordinateList (3UL<nrows>, 5UL<ncols>, [(1UL<rowindex>, 2UL<colindex>, 31); (2UL<rowindex>, 2UL<colindex>, 28)]), actual)

[<Fact>]
let ``mapi on empty matrix returns empty matrix`` () =
    let matrix = fromCoordinateList (CoordinateList (0UL<nrows>, 0UL<ncols>, []))
    let result = mapi matrix (fun i j (x: int option) ->
        match x with
        | Some v -> Some ((v + 1) * 3 + 2 * j)
        | None -> None
    )
    let actual = toCoordinateList result
    Assert.Equal(CoordinateList(0UL<nrows>, 0UL<ncols>, []), actual)

[<Fact>]
let ``mapi with special function returns empty matrix`` () =
    let coo = CoordinateList (5UL<nrows>, 7UL<ncols>, [(1UL<rowindex>, 2UL<colindex>, 15); (4UL<rowindex>,5UL<colindex>, 13)])
    let matrix = fromCoordinateList coo
    let result = mapi matrix (fun i j (x: int option) ->
        match x with
        | Some v ->
            match int (i + j) % 2 with
            | 0 -> Some (v * 2)
            | _ -> None
        | None -> None
    )
    let actual = toCoordinateList result
    Assert.Equal(CoordinateList(5UL<nrows>, 7UL<ncols>, []), actual)

[<Fact>]
let ``mapi works with function does not depend on the indexes`` () =
    let coo = CoordinateList (5UL<nrows>, 7UL<ncols>, [(1UL<rowindex>, 2UL<colindex>, 15); (4UL<rowindex>,5UL<colindex>, 13)])
    let matrix = fromCoordinateList coo
    let result = mapi matrix (fun i j (x: int option) ->
        match x with
        | Some v -> Some (v * 2)
        | None -> None
    )
    let actual = toCoordinateList result
    Assert.Equal(CoordinateList (5UL<nrows>, 7UL<ncols>, [(1UL<rowindex>, 2UL<colindex>, 30); (4UL<rowindex>,5UL<colindex>, 26)]), actual)

[<Fact>]
let ``slice returns error when row start is negative`` () =
    let matrix = fromCoordinateList (CoordinateList(5UL<nrows>, 6UL<ncols>, [(2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17)]))
    match slice matrix -1 4 2 3 with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("Start row should be >= 0", msg)

[<Fact>]
let ``slice returns error when row end is negative`` () =
    let matrix = fromCoordinateList (CoordinateList(5UL<nrows>, 6UL<ncols>, [(2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17)]))
    match slice matrix 1 -4 2 3 with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("End row should be >= 0", msg)

[<Fact>]
let ``slice returns error when col start is negative`` () =
    let matrix = fromCoordinateList (CoordinateList(5UL<nrows>, 6UL<ncols>, [(2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17)]))
    match slice matrix 1 4 -2 3 with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("Start column should be >= 0", msg)

[<Fact>]
let ``slice returns error when col end is negative`` () =
    let matrix = fromCoordinateList (CoordinateList(5UL<nrows>, 6UL<ncols>, [(2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17)]))
    match slice matrix 1 4 2 -3 with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("End column should be >= 0", msg)

[<Fact>]
let ``slice returns error when row start is out of range`` () =
    let matrix = fromCoordinateList (CoordinateList(5UL<nrows>, 6UL<ncols>, [(2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17)]))
    match slice matrix 6 4 2 3 with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("Start row is out of matrix length", msg)

[<Fact>]
let ``slice returns error when row end is out of range`` () =
    let matrix = fromCoordinateList (CoordinateList(5UL<nrows>, 6UL<ncols>, [(2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17)]))
    match slice matrix 1 10 2 3 with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("End row is out of matrix length", msg)

[<Fact>]
let ``slice returns error when col start is out of range`` () =
    let matrix = fromCoordinateList (CoordinateList(5UL<nrows>, 6UL<ncols>, [(2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17)]))
    match slice matrix 1 4 10 3 with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("Start column is out of matrix length", msg)

[<Fact>]
let ``slice returns error when col end is out of range`` () =
    let matrix = fromCoordinateList (CoordinateList(5UL<nrows>, 6UL<ncols>, [(2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17)]))
    match slice matrix 1 2 2 10 with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("End column is out of matrix length", msg)

[<Fact>]
let ``slice returns error when row end is less than row start`` () =
    let matrix = fromCoordinateList (CoordinateList(5UL<nrows>, 6UL<ncols>, [(2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17)]))
    match slice matrix 2 1 2 3 with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("Start row should be <= end row", msg)

[<Fact>]
let ``slice returns error when col end is less than col start`` () =
    let matrix = fromCoordinateList (CoordinateList(5UL<nrows>, 6UL<ncols>, [(2UL<rowindex>, 2UL<colindex>, 3); (3UL<rowindex>, 4UL<colindex>, 17)]))
    match slice matrix 1 2 3 2 with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("Start column should be <= end column", msg)

[<Fact>]
let ``slice returns correct square submatrix`` () =
    let matrix = fromCoordinateList (CoordinateList(7UL<nrows>, 7UL<ncols>, [(2UL<rowindex>, 2UL<colindex>, 33); (5UL<rowindex>, 5UL<colindex>, 28)]))
    match slice matrix 1 3 1 3 with
    | Result.Success result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(3UL<nrows>, 3UL<ncols>, [(1UL<rowindex>, 1UL<colindex>, 33)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns correct rectangular submatrix`` () =
    let matrix = fromCoordinateList (CoordinateList(5UL<nrows>, 9UL<ncols>, [(3UL<rowindex>, 7UL<colindex>, 33); (1UL<rowindex>, 2UL<colindex>, 28)]))
    match slice matrix 2 4 5 8 with
    | Result.Success result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(3UL<nrows>, 4UL<ncols>, [(1UL<rowindex>, 2UL<colindex>, 33)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns empty matrix`` () =
    let matrix = fromCoordinateList (CoordinateList(7UL<nrows>, 9UL<ncols>, [(3UL<rowindex>, 3UL<colindex>, 33); (1UL<rowindex>, 2UL<colindex>, 28)]))
    match slice matrix 4 6 5 8 with
    | Result.Success result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(3UL<nrows>, 4UL<ncols>, []), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns single submatrix`` () =
    let matrix = fromCoordinateList (CoordinateList(7UL<nrows>, 9UL<ncols>, [(3UL<rowindex>, 3UL<colindex>, 33); (1UL<rowindex>, 2UL<colindex>, 28)]))
    match slice matrix 1 1 2 2 with
    | Result.Success result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(1UL<nrows>, 1UL<ncols>, [(0UL<rowindex>, 0UL<colindex>, 28)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns correct submatrix equals to matrix`` () =
    let matrix = fromCoordinateList (CoordinateList(7UL<nrows>, 9UL<ncols>, [(3UL<rowindex>, 3UL<colindex>, 33); (1UL<rowindex>, 2UL<colindex>, 28)]))
    match slice matrix 0 6 0 8 with
    | Result.Success result ->
        let coo = toCoordinateList result
        printfn "Actual data: %A" coo.list
        Assert.Equal(CoordinateList(7UL<nrows>, 9UL<ncols>, [(1UL<rowindex>, 2UL<colindex>, 28); (3UL<rowindex>, 3UL<colindex>, 33)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns correct submatrix when row start of submatrix equals to row start of matrix`` () =
    let matrix = fromCoordinateList (CoordinateList(7UL<nrows>, 7UL<ncols>, [(0UL<rowindex>, 0UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 33); (6UL<rowindex>, 6UL<colindex>, 6)]))
    match slice matrix 0 4 0 4 with
    | Result.Success result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(5UL<nrows>, 5UL<ncols>, [(0UL<rowindex>, 0UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 33)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns correct submatrix when row end of submatrix equals to row end of matrix`` () =
    let matrix = fromCoordinateList (CoordinateList(7UL<nrows>, 7UL<ncols>, [(0UL<rowindex>, 0UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 33); (6UL<rowindex>, 6UL<colindex>, 6)]))
    match slice matrix 2 6 2 4 with
    | Result.Success result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(5UL<nrows>, 3UL<ncols>, [(1UL<rowindex>, 1UL<colindex>, 33)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns correct submatrix when col start of submatrix equals to col start of matrix`` () =
    let matrix = fromCoordinateList (CoordinateList(7UL<nrows>, 7UL<ncols>, [(0UL<rowindex>, 0UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 33); (6UL<rowindex>, 6UL<colindex>, 6)]))
    match slice matrix 2 5 0 6 with
    | Result.Success result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(4UL<nrows>, 7UL<ncols>, [(1UL<rowindex>, 3UL<colindex>, 33)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns correct submatrix when col end of submatrix equals to col end of matrix`` () =
    let matrix = fromCoordinateList (CoordinateList(7UL<nrows>, 7UL<ncols>, [(0UL<rowindex>, 0UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 33); (6UL<rowindex>, 6UL<colindex>, 6)]))
    match slice matrix 2 4 2 6 with
    | Result.Success result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(3UL<nrows>, 5UL<ncols>, [(1UL<rowindex>, 1UL<colindex>, 33)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns single column`` () =
    let matrix = fromCoordinateList (CoordinateList(10UL<nrows>, 10UL<ncols>, [(1UL<rowindex>, 3UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 2); (5UL<rowindex>, 7UL<colindex>, 3)]))
    match slice matrix 1 6 2 2 with
    | Result.Success result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(6UL<nrows>, 1UL<ncols>, [(1UL<rowindex>, 0UL<colindex>, 2)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns single row`` () =
    let matrix = fromCoordinateList (CoordinateList(10UL<nrows>, 10UL<ncols>, [(1UL<rowindex>, 3UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 2); (5UL<rowindex>, 7UL<colindex>, 3)]))
    match slice matrix 5 5 3 9 with
    | Result.Success result ->
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(1UL<nrows>, 7UL<ncols>, [(0UL<rowindex>, 4UL<colindex>, 3)]), coo)
    | Result.Failure msg -> failwith msg
