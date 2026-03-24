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

    Assert.Equal(expected, actual)

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

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.mapi. Length is power of two, multiply by index.`` () =
    let v =
        Vector.fromCoordinateList (
            Vector.CoordinateList(8UL<dataLength>, [ (0UL<index>, 1); (1UL<index>, 1); (2UL<index>, 1); (3UL<index>, 1); (4UL<index>, 2); (5UL<index>, 2); (6UL<index>, 2); (7UL<index>, 2) ])
        )

    let f (idx: uint64<index>) x =
        match x with
        | Some(a) -> Some(a * int idx)
        | _ -> None

    let expected =
        Vector.fromCoordinateList (
            Vector.CoordinateList(8UL<dataLength>, [ (0UL<index>, 0); (1UL<index>, 1); (2UL<index>, 2); (3UL<index>, 3); (4UL<index>, 8); (5UL<index>, 10); (6UL<index>, 12); (7UL<index>, 14) ])
        )

    let actual = Vector.mapi v f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.mapi. Length is not power of two.`` () =
    // Build vector [1, 1, 1, 1, 1, 1] with dummy at end
    let v =
        Vector.fromCoordinateList (
            Vector.CoordinateList(6UL<dataLength>, [ (0UL<index>, 1); (1UL<index>, 1); (2UL<index>, 1); (3UL<index>, 1); (4UL<index>, 1); (5UL<index>, 1) ])
        )

    // f idx x = x * idx
    let f (idx: uint64<index>) x =
        match x with
        | Some(a) -> Some(a * int idx)
        | _ -> None

    // Expected: [0, 1, 2, 3, 4, 5] (1*idx for each position)
    let expected =
        Vector.fromCoordinateList (
            Vector.CoordinateList(6UL<dataLength>, [ (0UL<index>, 0); (1UL<index>, 1); (2UL<index>, 2); (3UL<index>, 3); (4UL<index>, 4); (5UL<index>, 5) ])
        )

    let actual = Vector.mapi v f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.mapi. Uniform leaf expansion.`` () =
    // Vector of length 1 with value 5
    let v =
        let tree = Vector.btree.Leaf(UserValue(Some(5)))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 1UL<nvals>, store)

    // f idx x = x + idx (5 + 0 = 5)
    let f (idx: uint64<index>) x =
        match x with
        | Some(a) -> Some(a + int idx)
        | _ -> None

    let expected =
        let tree = Vector.btree.Leaf(UserValue(Some(5)))
        let store = Storage(1UL<storageSize>, tree)
        SparseVector(1UL<dataLength>, 1UL<nvals>, store)

    let actual = Vector.mapi v f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.mapi. All indices identity.`` () =
    // Vector with values matching their indices
    let v =
        Vector.fromCoordinateList (
            Vector.CoordinateList(
                4UL<dataLength>,
                [ (0UL<index>, 0); (2UL<index>, 2) ]
            )
        )

    let f (idx: uint64<index>) x =
        match x with
        | Some(a) when a = int idx -> Some a
        | _ -> None

    let actual = Vector.mapi v f
    let outputCL = Vector.toCoordinateList actual

    Assert.Equal(2UL<nvals>, actual.nvals)
    Assert.Equal<list<uint64<index> * int>>([ (0UL<index>, 0); (2UL<index>, 2) ], outputCL.data)


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

    Assert.Equal(expected, actual)

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

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.map2i. Length is power of two.`` () =
    let v1 =
        Vector.fromCoordinateList (
            Vector.CoordinateList(4UL<dataLength>, [ (0UL<index>, 1); (1UL<index>, 2); (2UL<index>, 3); (3UL<index>, 4) ])
        )

    let v2 =
        Vector.fromCoordinateList (
            Vector.CoordinateList(4UL<dataLength>, [ (0UL<index>, 10); (1UL<index>, 20); (2UL<index>, 30); (3UL<index>, 40) ])
        )

    let f idx x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b + int idx)
        | _ -> None

    let expected =
        Vector.fromCoordinateList (
            Vector.CoordinateList(4UL<dataLength>, [ (0UL<index>, 11); (1UL<index>, 23); (2UL<index>, 35); (3UL<index>, 47) ])
        )

    let actual = Vector.map2i v1 v2 f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.map2i. Length is not power of two.`` () =
    let v1 =
        Vector.fromCoordinateList (
            Vector.CoordinateList(6UL<dataLength>, [ (0UL<index>, 1); (1UL<index>, 2); (2UL<index>, 3); (3UL<index>, 4); (4UL<index>, 5); (5UL<index>, 6) ])
        )

    let v2 =
        Vector.fromCoordinateList (
            Vector.CoordinateList(6UL<dataLength>, [ (0UL<index>, 10); (1UL<index>, 10); (2UL<index>, 10); (3UL<index>, 10); (4UL<index>, 10); (5UL<index>, 10) ])
        )

    let f idx x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a * int idx + b)
        | _ -> None

    let expected =
        Vector.fromCoordinateList (
            Vector.CoordinateList(6UL<dataLength>, [ (0UL<index>, 10); (1UL<index>, 12); (2UL<index>, 16); (3UL<index>, 22); (4UL<index>, 30); (5UL<index>, 40) ])
        )

    let actual = Vector.map2i v1 v2 f

    Assert.Equal(expected, actual)

[<Fact>]
let ``Simple Vector.map2i. Mixed values.`` () =
    let v1 =
        Vector.fromCoordinateList (
            Vector.CoordinateList(4UL<dataLength>, [ (0UL<index>, 1); (2UL<index>, 3) ])
        )

    let v2 =
        Vector.fromCoordinateList (
            Vector.CoordinateList(4UL<dataLength>, [ (1UL<index>, 10); (3UL<index>, 30) ])
        )

    let f idx x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(a + b)
        | Some(a), None -> Some(a * 2)
        | None, Some(b) -> Some(b * 3)
        | _ -> None

    let actual = Vector.map2i v1 v2 f
    let actualCL = Vector.toCoordinateList actual

    Assert.Equal(4UL<nvals>, actual.nvals)

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


[<Fact>]    
let ``Gather``() =
    let data = 
        Vector.CoordinateList(
                5UL<dataLength>,
                [ (0UL<index>, 0.0)
                  (1UL<index>, 1.0)
                  (4UL<index>, 5.0) ]
        )
        |> Vector.fromCoordinateList

    let indices = 
        Vector.CoordinateList(
                5UL<dataLength>,
                [ (0UL<index>, 1UL<index>)
                  (1UL<index>, 4UL<index>)
                  (3UL<index>, 1UL<index>) ]
        )
        |> Vector.fromCoordinateList

    let actual = Vector.gather data indices

    let expected = 
        Vector.CoordinateList(
                5UL<dataLength>,
                [ (0UL<index>, 1.0)
                  (1UL<index>, 5.0)
                  (3UL<index>, 1.0) ]
        )
        |> Vector.fromCoordinateList

    Assert.Equal(expected, actual)
(*
[<Fact>]    
let ``Scatter``() =
    let data = 
        Vector.CoordinateList(
                5UL<dataLength>,
                [ (0UL<index>, 4.0)
                  (2UL<index>, 5.0)
                  ]
        )
        |> Vector.fromCoordinateList

    let indices = 
        Vector.CoordinateList(
                5UL<dataLength>,
                [ (0UL<index>, 3UL<index>)
                  (2UL<index>, 3UL<index>)
                  ]
        )
        |> Vector.fromCoordinateList

    let result = 
        Vector.CoordinateList(
                5UL<dataLength>,
                [ (3UL<index>, 1.0)
                  (4UL<index>, 3.0)
                  ]
        )
        |> Vector.fromCoordinateList
    let actual = Vector.scatter result data indices (fun x y -> match (x,y) with | (Some x, Some y) -> Some (x + y) | Some x, _ | _, Some x -> Some x | _ -> None)
    printVector actual
    let expected = 
        Vector.CoordinateList(
                5UL<dataLength>,
                [ (3UL<index>, 10.0)
                  (4UL<index>, 3.0)
                 ]
        )
        |> Vector.fromCoordinateList

    Assert.Equal(expected, actual)*)

let compare x y = 
    match (x,y)  with
    | Some x, None -> -1
    | Some x, Some y -> if x < y then -1 elif x > y then 1 else 0
    | None, Some x -> 1
    | _ -> 0

[<Fact>]    
let ``Sort one element vector``() =
    let data = 
        Vector.CoordinateList(
                1UL<dataLength>,
                [ (0UL<index>, 0.0)
                  ]
        )
        |> Vector.fromCoordinateList
    let actual = Vector.mergeSort data compare
    Assert.Equal(data, actual)

[<Fact>]    
let ``Sort vector of two equal elements``() =
    let data = 
        Vector.CoordinateList(
                2UL<dataLength>,
                [ (0UL<index>, 0.0);(1UL<index>, 0.0)
                  ]
        )
        |> Vector.fromCoordinateList
    let actual = Vector.mergeSort data compare
    Assert.Equal(data, actual)

[<Fact>]    
let ``Sort vector of three equal elements``() =
    let data = 
        Vector.CoordinateList(
                3UL<dataLength>,
                [ (0UL<index>, 2.0);(1UL<index>, 2.0);(2UL<index>, 2.0)
                  ]
        )
        |> Vector.fromCoordinateList
    
    let actual = Vector.mergeSort data compare
    Assert.Equal(data, actual)


[<Fact>]    
let ``Sort vector of three different unordered elements``() =
    let data = 
        Vector.CoordinateList(
                3UL<dataLength>,
                [ (0UL<index>, 2.0);(1UL<index>, 1.0);(2UL<index>, 4.0)
                  ]
        )
        |> Vector.fromCoordinateList
    let expected = 
        Vector.CoordinateList(
                3UL<dataLength>,
                [ (0UL<index>, 1.0);(1UL<index>, 2.0);(2UL<index>, 4.0)
                  ]
        )
        |> Vector.fromCoordinateList
    let actual = Vector.mergeSort data compare
    Assert.Equal(expected, actual)
    


[<Fact>]    
let ``Sort long vector with one element``() =
    let data = 
        Vector.CoordinateList(
                5UL<dataLength>,
                [ (0UL<index>, 0.0)
                  ]
        )
        |> Vector.fromCoordinateList
    let actual = Vector.mergeSort data compare
    printVector actual
    Assert.Equal(data, actual)



[<Fact>]    
let ``Sort sorted vector``() =
    let data = 
        Vector.CoordinateList(
                5UL<dataLength>,
                [ (0UL<index>, 0.0); (1UL<index>, 0.0)
                  ]
        )
        |> Vector.fromCoordinateList
    let actual = Vector.mergeSort data compare
    Assert.Equal(data, actual)

    