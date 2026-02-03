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
