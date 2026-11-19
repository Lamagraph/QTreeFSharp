module LinearAlgebra.COO.Matrix.Tests

open Xunit
open FsCheck.Xunit

open LinearAlgebra.Common
open LinearAlgebra.COO.Tests.Helpers

module CMatrix = LinearAlgebra.COO.Matrix
module QMatrix = LinearAlgebra.QuadTree.Matrix

[<Fact>]
let ``fromCoordinateList sorts row-major and keeps the last duplicate`` () =
    let actual =
        CMatrix.CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [ 2UL<rowindex>, 0UL<colindex>, 'a'
              0UL<rowindex>, 2UL<colindex>, 'b'
              0UL<rowindex>, 1UL<colindex>, 'c'
              2UL<rowindex>, 0UL<colindex>, 'd' ]
        )
        |> CMatrix.fromCoordinateList
        |> unwrap

    Assert.Equal<CMatrix.COOEntry<char> list>(
        [ 0UL<rowindex>, 1UL<colindex>, 'c'
          0UL<rowindex>, 2UL<colindex>, 'b'
          2UL<rowindex>, 0UL<colindex>, 'd' ],
        actual.data
    )

[<Fact>]
let ``fromCoordinateList rejects out of range coordinates`` () =
    let actual =
        CMatrix.CoordinateList(2UL<nrows>, 3UL<ncols>, [ 0UL<rowindex>, 3UL<colindex>, 1 ])
        |> CMatrix.fromCoordinateList

    Assert.Equal(Error CMatrix.CoordinatesOutOfRange, actual)
    Assert.Equal("Coordinates out of range", string CMatrix.CoordinatesOutOfRange)

[<Fact>]
let ``map2 rejects matrices of different shape`` () =
    let actual =
        CMatrix.map2 (CMatrix.empty 2UL<nrows> 3UL<ncols>) (CMatrix.empty 3UL<nrows> 2UL<ncols>) plus

    Assert.Equal(Error CMatrix.InconsistentSizeOfArguments, actual)

[<Fact>]
let ``transposeUnsorted only swaps coordinates`` () =
    let m =
        cMatrix
            2UL
            3UL
            [ 0UL<rowindex>, 1UL<colindex>, 1
              0UL<rowindex>, 2UL<colindex>, 2
              1UL<rowindex>, 0UL<colindex>, 3 ]

    let t = CMatrix.transposeUnsorted m
    Assert.Equal(3UL<nrows>, t.nrows)
    Assert.Equal(2UL<ncols>, t.ncols)

    Assert.Equal<CMatrix.COOEntry<int> list>(
        [ 1UL<rowindex>, 0UL<colindex>, 1
          2UL<rowindex>, 0UL<colindex>, 2
          0UL<rowindex>, 1UL<colindex>, 3 ],
        t.data
    )

    Assert.Equal(CMatrix.sort t, CMatrix.transpose m)

let private sparseF (x: int option) (y: int option) =
    match x, y with
    | Some a, Some b -> Some(a * b)
    | Some a, None -> Some a
    | None, Some b -> if b > 0 then Some b else None
    | None, None -> None

let private denseF (x: int option) (y: int option) =
    match x, y with
    | None, None -> Some 0
    | _ -> sparseF x y

[<Property>]
let ``map2 equals quad-tree``
    (r: byte)
    (c: byte)
    (e1: (byte * byte * int) list)
    (e2: (byte * byte * int) list)
    (dense: bool)
    =
    let nrows, ncols = dim r, dim c
    let d1, d2 = matrixData nrows ncols e1, matrixData nrows ncols e2
    let f = if dense then denseF else sparseF

    let expected =
        QMatrix.map2 (qMatrix nrows ncols d1) (qMatrix nrows ncols d2) f
        |> unwrap
        |> qMatrixData

    let actual =
        CMatrix.map2 (cMatrix nrows ncols d1) (cMatrix nrows ncols d2) f |> unwrap

    expected = actual.data

[<Property>]
let ``mask equals quad-tree`` (r: byte) (c: byte) (e1: (byte * byte * int) list) (e2: (byte * byte * int) list) =
    let nrows, ncols = dim r, dim c
    let d1, d2 = matrixData nrows ncols e1, matrixData nrows ncols e2

    let expected =
        QMatrix.mask (qMatrix nrows ncols d1) (qMatrix nrows ncols d2) Option.isSome
        |> unwrap
        |> qMatrixData

    let actual =
        CMatrix.mask (cMatrix nrows ncols d1) (cMatrix nrows ncols d2) Option.isSome
        |> unwrap

    expected = actual.data

[<Property>]
let ``transpose, getLowerTriangle and foldAssociative equal quad-tree``
    (r: byte)
    (c: byte)
    (e: (byte * byte * int) list)
    =
    let nrows, ncols = dim r, dim c
    let d = matrixData nrows ncols e
    let q, m = qMatrix nrows ncols d, cMatrix nrows ncols d

    qMatrixData (QMatrix.transpose q) = (CMatrix.transpose m).data
    && qMatrixData (QMatrix.getLowerTriangle q) = (CMatrix.getLowerTriangle m).data
    && QMatrix.foldAssociative plus None q = CMatrix.foldAssociative plus None m

[<Property>]
let ``unsorted twins equal sorted ops after sort``
    (r: byte)
    (c: byte)
    (e1: (byte * byte * int) list)
    (e2: (byte * byte * int) list)
    (seed: int)
    =
    let nrows, ncols = dim r, dim c

    let m1, m2 =
        cMatrix nrows ncols (matrixData nrows ncols e1), cMatrix nrows ncols (matrixData nrows ncols e2)

    let s1, s2 = shuffleMatrix seed m1, shuffleMatrix (seed + 1) m2

    let fromUnsorted =
        CMatrix.CoordinateList(nrows * 1UL<nrows>, ncols * 1UL<ncols>, shuffle seed (matrixData nrows ncols e1))
        |> CMatrix.fromCoordinateListUnsorted
        |> unwrap

    CMatrix.sort (CMatrix.map2Unsorted s1 s2 sparseF |> unwrap) = (CMatrix.map2 m1 m2 sparseF |> unwrap)
    && CMatrix.sort (CMatrix.map2Unsorted s1 s2 denseF |> unwrap) = (CMatrix.map2 m1 m2 denseF |> unwrap)
    && CMatrix.sort (CMatrix.maskUnsorted s1 s2 Option.isNone |> unwrap) = (CMatrix.mask m1 m2 Option.isNone |> unwrap)
    && CMatrix.sort (CMatrix.transposeUnsorted s1) = CMatrix.transpose m1
    && CMatrix.sort (CMatrix.getLowerTriangleUnsorted s1) = CMatrix.getLowerTriangle m1
    && CMatrix.foldAssociativeUnsorted plus None s1 = CMatrix.foldAssociative plus None m1
    && CMatrix.sort fromUnsorted = m1
