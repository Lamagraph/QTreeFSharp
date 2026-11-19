module LinearAlgebra.COO.Vector.Tests

open Xunit
open FsCheck.Xunit

open LinearAlgebra.Common
open LinearAlgebra.COO.Tests.Helpers

module CVector = LinearAlgebra.COO.Vector
module QVector = LinearAlgebra.QuadTree.Vector

[<Fact>]
let ``fromCoordinateList sorts and keeps the last duplicate`` () =
    let actual =
        CVector.CoordinateList(5UL<dataLength>, [ 3UL<index>, 'a'; 1UL<index>, 'b'; 3UL<index>, 'c' ])
        |> CVector.fromCoordinateList

    Assert.Equal(
        Ok
            { CVector.SparseVector.length = 5UL<dataLength>
              CVector.SparseVector.data = [ 1UL<index>, 'b'; 3UL<index>, 'c' ] },
        actual
    )

[<Fact>]
let ``fromCoordinateList rejects out of range index`` () =
    let actual =
        CVector.CoordinateList(3UL<dataLength>, [ 3UL<index>, 1 ])
        |> CVector.fromCoordinateList

    Assert.Equal(Error CVector.CoordinatesOutOfRange, actual)
    Assert.Equal("Index out of range", string CVector.CoordinatesOutOfRange)

[<Fact>]
let ``map2 rejects vectors of different length`` () =
    let actual =
        CVector.map2 (CVector.empty 3UL<dataLength>) (CVector.empty 4UL<dataLength>) plus

    Assert.Equal(Error CVector.InconsistentSizeOfArguments, actual)

[<Fact>]
let ``nvals counts stored values`` () =
    Assert.Equal(2UL<nvals>, CVector.nvals (cVector 4UL [ 0UL<index>, 1; 2UL<index>, 2 ]))

[<Fact>]
let ``unsafeGet works on unsorted data`` () =
    let v: CVector.SparseVector<int> =
        { length = 4UL<dataLength>
          data = [ 3UL<index>, 30; 0UL<index>, 0 ] }

    Assert.Equal(Some 0, CVector.unsafeGet v 0UL<index>)
    Assert.Equal(None, CVector.unsafeGet v 1UL<index>)

/// Pitfall: a sorted operation given unsorted data duplicates keys and never pairs 0 with 0.
[<Fact>]
let ``map2 on unsorted input produces duplicate keys (documented pitfall)`` () =
    let v1: CVector.SparseVector<int> =
        { length = 3UL<dataLength>
          data = [ 2UL<index>, 1; 0UL<index>, 2 ] }

    let v2 = cVector 3UL [ 0UL<index>, 10 ]

    let wrong = CVector.map2 v1 v2 plus |> unwrap
    Assert.Equal<(uint64<index> * int) list>([ 0UL<index>, 10; 2UL<index>, 1; 0UL<index>, 2 ], wrong.data)

    let right = CVector.map2Unsorted v1 v2 plus |> unwrap |> CVector.sort
    Assert.Equal<(uint64<index> * int) list>([ 0UL<index>, 12; 2UL<index>, 1 ], right.data)

let private sparseF (x: int option) (y: int option) =
    match x, y with
    | Some a, Some b -> Some(a - b)
    | Some a, None -> Some a
    | None, Some b -> if b % 2 = 0 then Some b else None
    | None, None -> None

/// `f None None = Some`: the result is dense.
let private denseF (x: int option) (y: int option) =
    match x, y with
    | None, None -> Some 7
    | _ -> sparseF x y

let private mapiF (i: uint64<index>) (x: int option) =
    match x with
    | Some a -> Some(a + int i)
    | None -> if i % 3UL<index> = 0UL<index> then Some -1 else None

[<Property>]
let ``map2 equals quad-tree`` (n: byte) (e1: (byte * int) list) (e2: (byte * int) list) (dense: bool) =
    let len = dim n
    let d1, d2 = vectorData len e1, vectorData len e2
    let f = if dense then denseF else sparseF

    let expected =
        QVector.map2 (qVector len d1) (qVector len d2) f |> unwrap |> qVectorData

    let actual = CVector.map2 (cVector len d1) (cVector len d2) f |> unwrap
    expected = actual.data

[<Property>]
let ``mask equals quad-tree`` (n: byte) (e1: (byte * int) list) (e2: (byte * int) list) =
    let len = dim n
    let d1, d2 = vectorData len e1, vectorData len e2
    let f = Option.isNone

    let expected =
        QVector.mask (qVector len d1) (qVector len d2) f |> unwrap |> qVectorData

    let actual = CVector.mask (cVector len d1) (cVector len d2) f |> unwrap
    expected = actual.data

[<Property>]
let ``mapi equals quad-tree`` (n: byte) (e: (byte * int) list) =
    let len = dim n
    let d = vectorData len e

    let expected = QVector.mapi (qVector len d) mapiF |> qVectorData
    let actual = CVector.mapi (cVector len d) mapiF
    expected = actual.data

[<Property>]
let ``unsorted twins equal sorted ops after sort``
    (n: byte)
    (e1: (byte * int) list)
    (e2: (byte * int) list)
    (seed: int)
    =
    let len = dim n
    let v1, v2 = cVector len (vectorData len e1), cVector len (vectorData len e2)
    let s1, s2 = shuffleVector seed v1, shuffleVector (seed + 1) v2

    let fromUnsorted =
        CVector.CoordinateList(len * 1UL<dataLength>, shuffle seed (vectorData len e1))
        |> CVector.fromCoordinateListUnsorted
        |> unwrap

    CVector.sort (CVector.map2Unsorted s1 s2 sparseF |> unwrap) = (CVector.map2 v1 v2 sparseF |> unwrap)
    && CVector.sort (CVector.map2Unsorted s1 s2 denseF |> unwrap) = (CVector.map2 v1 v2 denseF |> unwrap)
    && CVector.sort (CVector.maskUnsorted s1 s2 Option.isSome |> unwrap) = (CVector.mask v1 v2 Option.isSome |> unwrap)
    && CVector.sort (CVector.mapiUnsorted s1 mapiF) = CVector.mapi v1 mapiF
    && CVector.sort fromUnsorted = v1
