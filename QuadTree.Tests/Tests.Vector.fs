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
let ``map on empty vector returns empty vector`` () =
    let vec = fromCoordinateList (CoordinateList(0UL<dataLength>, []))
    let result = map vec (fun (x: int option) -> 
        match x with
        | Some v -> Some (v * 3)
        | None -> None)
    let coo = toCoordinateList result
    Assert.Equal(CoordinateList(0UL<dataLength>, []), coo)

[<Fact>]
let ``map with function that turns all the elements into zeros`` () =
    let vec = fromCoordinateList (CoordinateList(7UL<dataLength>, [(1UL<index>, 3); (2UL<index>, 3); (5UL<index>, 100)]))
    let result = map vec (fun _ -> Some 0)
    let coo = toCoordinateList result
    Assert.Equal(CoordinateList(7UL<dataLength>, [(0UL<index>, 0); (1UL<index>, 0); (2UL<index>, 0); (3UL<index>, 0); (4UL<index>, 0); (5UL<index>, 0); (6UL<index>, 0)]), coo)

[<Fact>]
let ``map with function that resets all the elements`` () =
    let vec = fromCoordinateList (CoordinateList(7UL<dataLength>, [(1UL<index>, 3); (2UL<index>, 3); (5UL<index>, 100)]))
    let result = map vec (fun _ -> None)
    let coo = toCoordinateList result
    Assert.Equal(CoordinateList(7UL<dataLength>, []), coo)

[<Fact>]
let ``map can change type from int to string`` () =
    let vec = fromCoordinateList (CoordinateList(3UL<dataLength>, [(1UL<index>, 11); (2UL<index>, 33)]))
    let result = map vec (fun (x: int option) -> 
        match x with
        | Some v -> Some (sprintf "str %d" v)
        | None -> None)
    let coo = toCoordinateList result
    Assert.Equal(CoordinateList(3UL<dataLength>, [(1UL<index>, "str 11"); (2UL<index>, "str 33")]), coo)

[<Fact>]
let ``fromCoordinateList with index out of range`` () =
    let coo = CoordinateList(6UL<dataLength>, [(9UL<index>, 8)])
    let vec = fromCoordinateList coo
    let result = toCoordinateList vec
    Assert.Equal(CoordinateList(6UL<dataLength>, []), result)

[<Fact>]
let ``fromCoordinateList with unsorted coordinates works correctly`` () =
    let coo = CoordinateList(7UL<dataLength>, [(5UL<index>, 3); (3UL<index>, 2); (1UL<index>, 100)])
    let vec = fromCoordinateList coo
    let result = toCoordinateList vec
    Assert.Equal(CoordinateList(7UL<dataLength>, [(1UL<index>, 100); (3UL<index>, 2); (5UL<index>, 3)]), result)
(*
[<Fact>]
let ``fromCoordinateList with duplicate indicies returns the last of them`` () =
    let coo = CoordinateList(3UL<dataLength>, [(1UL<index>, 33); (1UL<index>, 100)])
    let vec = Vector.fromCoordinateList coo
    let result = Vector.toCoordinateList vec
    Assert.Equal(CoordinateList(3UL<dataLength>, [(1UL<index>, 100)]), result)
*)
[<Fact>]
let ``fromCoordinateList with zero length and some values returns empty vector`` () =
    let coo = CoordinateList(0UL<dataLength>, [(33UL<index>, 33); (39UL<index>, 1)])
    let vec = fromCoordinateList coo
    let result = toCoordinateList vec
    Assert.Equal(CoordinateList(0UL<dataLength>, []), result)

[<Fact>]
let ``mapi works with index`` () =
    let vec = fromCoordinateList (CoordinateList(5UL<dataLength>, [(2UL<index>, 10); (3UL<index>, 23)]))
    let result = mapi vec (fun (i: uint64<index>) (x: int option) -> 
        match x with
        | Some v -> Some (int i + v)
        | None -> None
    )
    let coo = toCoordinateList result
    Assert.Equal(CoordinateList(5UL<dataLength>, [(2UL<index>, 12); (3UL<index>, 26)]), coo)

[<Fact>]
let ``mapi on empty vector returns empty vector`` () =
    let vec = fromCoordinateList (CoordinateList(0UL<dataLength>, []))
    let result = mapi vec (fun (i: uint64<index>) (x: int option) -> 
        match x with
        | Some v -> Some (int i + v)
        | None -> None
    )
    let coo = toCoordinateList result
    Assert.Equal(CoordinateList(0UL<dataLength>, []), coo)

[<Fact>]
let ``mapi with special function returns empty vector`` () =
    let vec = fromCoordinateList (CoordinateList(5UL<dataLength>, [(1UL<index>, 33); (3UL<index>, 88)]))
    let result = mapi vec (fun (i: uint64<index>) (x: int option) -> 
        match x with
        | Some v ->
            match int i % 2 with
            | 0 -> Some (v * 2)
            | _ -> None
        | None -> None
    )
    let coo = toCoordinateList result
    Assert.Equal(CoordinateList(5UL<dataLength>, []), coo)

[<Fact>]
let ``mapi works with function does not depend on the index`` () =
    let vec = fromCoordinateList (CoordinateList(5UL<dataLength>, [(2UL<index>, 10); (3UL<index>, 23)]))
    let result = mapi vec (fun (i: uint64<index>) (x: int option) -> 
        match x with
        | Some v -> Some (v + 4)
        | None -> None
    )
    let coo = toCoordinateList result
    Assert.Equal(CoordinateList(5UL<dataLength>, [(2UL<index>, 14); (3UL<index>, 27)]), coo)

[<Fact>]
let ``slice returns error when start is negative`` () =
    let vec = fromCoordinateList (CoordinateList(5UL<dataLength>, [(2UL<index>, 10)]))
    match slice -1 3 vec with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("Start should be >= 0", msg)

[<Fact>]
let ``slice returns error when end is negative`` () =
    let vec = fromCoordinateList (CoordinateList(5UL<dataLength>, [(2UL<index>, 10)]))
    match slice 1 -3 vec with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("End should be >= 0", msg)

[<Fact>]
let ``slice returns error when start is out of range`` () =
    let vec = fromCoordinateList (CoordinateList(5UL<dataLength>, [(2UL<index>, 10)]))
    match slice 7 3 vec with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("Start is out of Vector length", msg)

[<Fact>]
let ``slice returns error when end is out of range`` () =
    let vec = fromCoordinateList (CoordinateList(5UL<dataLength>, [(2UL<index>, 10)]))
    match slice 3 7 vec with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("End is out of Vector length", msg)

[<Fact>]
let ``slice returns error when end is less than start`` () =
    let vec = fromCoordinateList (CoordinateList(5UL<dataLength>, [(2UL<index>, 10)]))
    match slice 4 3 vec with
    | Result.Success _ -> failwith "Expected Error"
    | Result.Failure msg -> Assert.Equal("End should be >= Start", msg)

[<Fact>]
let ``slice returns correct subvector`` () =
    let vec = fromCoordinateList (CoordinateList(7UL<dataLength>, [(2UL<index>, 10); (6UL<index>, 20)]))
    match slice 1 3 vec with
    | Result.Success result -> 
        let coo = Vector.toCoordinateList result
        Assert.Equal(CoordinateList(3UL<dataLength>, [(1UL<index>, 10)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns subvector without elements`` () =
    let vec = fromCoordinateList (CoordinateList(7UL<dataLength>, [(5UL<index>, 10); (6UL<index>, 20)]))
    match slice 0 2 vec with
    | Result.Success result -> 
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(3UL<dataLength>, []), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns single subvector`` () =
    let vec = fromCoordinateList (CoordinateList(7UL<dataLength>, [(2UL<index>, 10); (6UL<index>, 20)]))
    match slice 2 2 vec with
    | Result.Success result -> 
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(1UL<dataLength>, [(0UL<index>, 10)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns correct subvector equals to vector`` () =
    let vec = fromCoordinateList (CoordinateList(7UL<dataLength>, [(2UL<index>, 10); (3UL<index>, 33); (6UL<index>, 20)]))
    match slice 0 6 vec with
    | Result.Success result -> 
        let coo = toCoordinateList result
        Assert.Equal(CoordinateList(7UL<dataLength>, [(2UL<index>, 10); (3UL<index>, 33); (6UL<index>, 20)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns correct subvector when start of subvector equals to start of vector`` () =
    let vec = fromCoordinateList (CoordinateList(7UL<dataLength>, [(0UL<index>, 10); (3UL<index>, 33); (6UL<index>, 20)]))
    match slice 0 4 vec with
    | Result.Success result -> 
        let coo = Vector.toCoordinateList result
        Assert.Equal(CoordinateList(5UL<dataLength>, [(0UL<index>, 10); (3UL<index>, 33)]), coo)
    | Result.Failure msg -> failwith msg

[<Fact>]
let ``slice returns correct subvector when end of subvector equals to end of vector`` () =
    let vec = fromCoordinateList (CoordinateList(7UL<dataLength>, [(0UL<index>, 10); (3UL<index>, 33); (6UL<index>, 20)]))
    match slice 2 6 vec with
    | Result.Success result -> 
        let coo = Vector.toCoordinateList result
        Assert.Equal(CoordinateList(5UL<dataLength>, [(1UL<index>, 33); (4UL<index>, 20)]), coo)
    | Result.Failure msg -> failwith msg
