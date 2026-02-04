module Vector.Tests

open System
open Xunit

open Vector
open Common

let printVector (vector: SparseVector<_>) =
    printfn "Vector:"
    printfn "   Length: %A" vector.length
    printfn "   Nvals: %A" vector.nvals
    printfn "   Storage:"
    printfn "      Size: %A" vector.storage.size
    printfn "      Data: %A" vector.storage.data


[<Fact>]
let ``Simple Vector.map. Length is power of two.`` () =
    let v =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Leaf(UserValue(Some(2)))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(8UL<dataLength>, 6UL<nvals>, store)

    let f x =
        match x with
        | Some(a) -> Some(a * 2)
        | _ -> None

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(2))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Leaf(UserValue(Some(4)))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(8UL<dataLength>, 6UL<nvals>, store)

    let actual = Vector.map v f

    let eq = actual = expected

    Assert.True(eq)

[<Fact>]
let ``Simple Vector.map. Length is not power of two.`` () =
    let v =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(Dummy))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(6UL<dataLength>, 2UL<nvals>, store)

    let f x =
        match x with
        | Some(a) -> Some(a * 2)
        | _ -> None

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(2))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(Dummy))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(6UL<dataLength>, 2UL<nvals>, store)

    let actual = Vector.map v f

    let eq = actual = expected

    Assert.True(eq)


[<Fact>]
let ``Simple Vector.map2. Length is power of two.`` () =
    let v1 =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Leaf(UserValue(Some(2)))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(8UL<dataLength>, 6UL<nvals>, store)

    let v2 =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(2))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(UserValue(Some(1))))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(8UL<dataLength>, 4UL<nvals>, store)

    let f x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | _ -> None

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(3))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(UserValue(Some(3))))
            )

        let store = Storage(8UL<storageSize>, tree)
        Result.Success(SparseVector(8UL<dataLength>, 4UL<nvals>, store))

    let actual = Vector.map2 v1 v2 f

    let eq = actual = expected

    Assert.True(eq)

[<Fact>]
let ``Simple Vector.map2. Length is not power of two.`` () =
    let v1 =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(Dummy))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(6UL<dataLength>, 2UL<nvals>, store)

    let v2 =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(2))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(Dummy))
            )

        let store = Storage(8UL<storageSize>, tree)
        SparseVector(6UL<dataLength>, 2UL<nvals>, store)

    let f x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | _ -> None

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(3))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(None)), Vector.btree.Leaf(Dummy))
            )

        let store = Storage(8UL<storageSize>, tree)
        Result.Success(SparseVector(6UL<dataLength>, 2UL<nvals>, store))

    let actual = Vector.map2 v1 v2 f

    let eq = actual = expected

    Assert.True(eq)

[<Fact>]
let ``Conversion identity`` () =
    let id = toCoordinateList << fromCoordinateList

    let dataLength = 10UL<dataLength>

    let data =
        [ 0UL<index>, 3; 3UL<index>, -1; 7UL<index>, 2; 8UL<index>, 2; 9UL<index>, 2 ]

    let coordinates = CoordinateList(dataLength, data)

    let expected = coordinates
    let actual = id coordinates

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple addition`` () =
    let dataLength = 10UL<dataLength>

    let d1 = [ 0UL<index>, 2; 9UL<index>, 1 ]
    let d2 = [ 0UL<index>, 3; 8UL<index>, 1 ]

    let expected =
        let expectedList = [ 0UL<index>, 5; 8UL<index>, 1; 9UL<index>, 1 ]
        CoordinateList(dataLength, expectedList)

    let actual =
        let c1 = CoordinateList(dataLength, d1)
        let c2 = CoordinateList(dataLength, d2)
        let v1 = fromCoordinateList c1
        let v2 = fromCoordinateList c2

        let addition o1 o2 =
            match o1, o2 with
            | Some x, Some y -> Some(x + y)
            | Some x, None
            | None, Some x -> Some x
            | None, None -> None

        let result =
            match map2 v1 v2 addition with
            | Result.Success x -> x
            | _ -> failwith "Unreachable"

        toCoordinateList result

    Assert.Equal(expected, actual)

[<Fact>]
let ``Condensation of empty`` () =
    let clist = CoordinateList(10UL<dataLength>, [])

    let actual = fromCoordinateList clist

    // 16 elements total None and Dummy: NNNNNNNN | NN DD | DDDD
    let tree =

        btree.Node(
            btree.Leaf <| UserValue None,
            btree.Node(btree.Node(btree.Leaf <| UserValue None, btree.Leaf Dummy), btree.Leaf Dummy)
        )

    let expected =
        SparseVector(clist.length, 0UL<nvals>, Storage(16UL<storageSize>, tree))

    Assert.Equal(expected, actual)
