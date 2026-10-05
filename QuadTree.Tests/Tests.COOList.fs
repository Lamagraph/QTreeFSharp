module COOList.Tests

open System
open Xunit

open Matrix
open COOList
open Common

let op_add x y =
    match (x, y) with
    | Some(a), Some(b) -> Some(a + b)
    | Some a, None
    | None, Some a -> Some a
    | _ -> None

let op_mult x y =
    match (x, y) with
    | Some(a), Some(b) -> Some(a * b)
    | _ -> None

let private keysAscending (entries: COOEntry<'v> list) =
    entries
    |> List.map (fun (i, j, _) -> (i, j))
    |> List.pairwise
    |> List.forall (fun ((a1, b1), (a2, b2)) -> a1 < a2 || (a1 = a2 && b1 < b2))

// === cooGet tests ===

[<Fact>]
let ``cooGet existing value`` () =
    let coo =
        ListCOO(
            4UL<nrows>,
            4UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (0UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 0UL<colindex>, 3) ]
        )

    let actual = cooGet (coo, 0UL<rowindex>, 1UL<colindex>)

    Assert.Equal(Ok(Some 2), actual)

[<Fact>]
let ``cooGet missing value`` () =
    let coo =
        ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let actual = cooGet (coo, 2UL<rowindex>, 2UL<colindex>)

    Assert.Equal(Ok None, actual)

[<Fact>]
let ``cooGet out of bounds`` () =
    let coo = ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    Assert.Throws<System.ArgumentOutOfRangeException>(fun () -> cooGet (coo, 5UL<rowindex>, 5UL<colindex>) |> ignore)

// === cooUpdate tests ===

[<Fact>]
let ``cooUpdate replaces existing`` () =
    let coo =
        ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let expected =
        ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 99); (1UL<rowindex>, 1UL<colindex>, 2) ])

    let actual = cooUpdate (coo, 0UL<rowindex>, 0UL<colindex>, 99)

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooUpdate inserts new in middle`` () =
    let coo =
        ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 2) ])

    let expected =
        ListCOO(
            4UL<nrows>,
            4UL<ncols>,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (1UL<rowindex>, 1UL<colindex>, 10)
              (2UL<rowindex>, 2UL<colindex>, 2) ]
        )

    let actual = cooUpdate (coo, 1UL<rowindex>, 1UL<colindex>, 10)

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooUpdate inserts at end`` () =
    let coo = ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    let expected =
        ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1); (3UL<rowindex>, 3UL<colindex>, 20) ])

    let actual = cooUpdate (coo, 3UL<rowindex>, 3UL<colindex>, 20)

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooUpdate out of bounds`` () =
    let coo = ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    Assert.Throws<System.ArgumentOutOfRangeException>(fun () ->
        cooUpdate (coo, 5UL<rowindex>, 5UL<colindex>, 99) |> ignore)

// === cooMap tests ===

[<Fact>]
let ``cooMap doubles values`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let data =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 2)
          (1UL<rowindex>, 0UL<colindex>, 3)
          (1UL<rowindex>, 1UL<colindex>, 4) ]
        |> List.sort

    let coo = ListCOO(nrows, ncols, data)

    let f v = v |> Option.map (fun v -> v * 2)

    let expected =
        ListCOO(
            nrows,
            ncols,
            [ (0UL<rowindex>, 0UL<colindex>, 2)
              (0UL<rowindex>, 1UL<colindex>, 4)
              (1UL<rowindex>, 0UL<colindex>, 6)
              (1UL<rowindex>, 1UL<colindex>, 8) ]
        )

    let actual = cooMap coo f

    Assert.Equal(expected, actual)

[<Fact>]
let ``cooMap filters None results`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let data =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 2)
          (1UL<rowindex>, 0UL<colindex>, 3)
          (1UL<rowindex>, 1UL<colindex>, 4) ]

    let coo = ListCOO(nrows, ncols, data)

    let f v =
        v
        |> Option.bind (fun v ->
            match v with
            | 1 -> None
            | _ -> Some(v * 10))

    let expected =
        ListCOO(
            nrows,
            ncols,
            [ (0UL<rowindex>, 1UL<colindex>, 20)
              (1UL<rowindex>, 0UL<colindex>, 30)
              (1UL<rowindex>, 1UL<colindex>, 40) ]
        )

    let actual = cooMap coo f

    Assert.Equal(expected, actual)

[<Fact>]
let ``cooMap and cooMapi fill missing cells (general form)`` () =
    let nrows = 3UL<nrows>
    let ncols = 3UL<ncols>

    let data = [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 5) ]

    let coo = ListCOO(nrows, ncols, data)

    let f v = Some(defaultArg v 0)

    for actual in [ cooMap coo f; cooMapi coo (fun _ _ v -> f v) ] do
        Assert.Equal(nrows, actual.nrows)
        Assert.Equal(ncols, actual.ncols)
        Assert.Equal(9, actual.entries.Length)

        Assert.Equal(
            List.tryFind (fun (i, j, _) -> i = 2UL<rowindex> && j = 2UL<colindex>) actual.entries,
            Some(2UL<rowindex>, 2UL<colindex>, 5)
        )

[<Fact>]
let ``cooMap and cooMapi on zero-size matrix`` () =
    let coo = ListCOO(0UL<nrows>, 0UL<ncols>, [])
    let f v = v |> Option.map (fun v -> v * 2)
    let expected = ListCOO(0UL<nrows>, 0UL<ncols>, [])

    Assert.Equal(expected, cooMap coo f)
    Assert.Equal(expected, cooMapi coo (fun _ _ v -> f v))

// === cooMap2 tests ===

[<Fact>]
let ``cooMap2 addition`` () =
    let nrows = 10UL<nrows>
    let ncols = 12UL<ncols>

    let d1 =
        [ (0UL<rowindex>, 3UL<colindex>, 4)
          (3UL<rowindex>, 11UL<colindex>, 2)
          (9UL<rowindex>, 2UL<colindex>, 5) ]
        |> List.sort

    let d2 =
        [ (0UL<rowindex>, 3UL<colindex>, 6)
          (3UL<rowindex>, 3UL<colindex>, 33)
          (3UL<rowindex>, 11UL<colindex>, -1) ]
        |> List.sort

    let f x y =
        match x, y with
        | Some a, Some b -> Some(a + b)
        | Some a, None -> Some a
        | None, Some b -> Some b
        | _ -> None

    let expected =
        ListCOO(
            nrows,
            ncols,
            [ (0UL<rowindex>, 3UL<colindex>, 10)
              (3UL<rowindex>, 3UL<colindex>, 33)
              (9UL<rowindex>, 2UL<colindex>, 5)
              (3UL<rowindex>, 11UL<colindex>, 1) ]
            |> List.sort
        )

    let c1 = ListCOO(nrows, ncols, d1)
    let c2 = ListCOO(nrows, ncols, d2)

    let actual = cooMap2 c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooMap2 with mismatched positions`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 = [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 3) ]

    let d2 = [ (1UL<rowindex>, 1UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 30) ]

    let f x y =
        match x, y with
        | Some a, Some b -> Some(a + b)
        | Some a, None -> Some(a + 100)
        | None, Some b -> Some(b + 200)
        | _ -> None

    let expected =
        ListCOO(
            nrows,
            ncols,
            [ (0UL<rowindex>, 0UL<colindex>, 101)
              (1UL<rowindex>, 1UL<colindex>, 210)
              (2UL<rowindex>, 2UL<colindex>, 103)
              (3UL<rowindex>, 3UL<colindex>, 230) ]
        )

    let c1 = ListCOO(nrows, ncols, d1)
    let c2 = ListCOO(nrows, ncols, d2)

    let actual = cooMap2 c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooMap2 dense filters None from existing entries`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 = [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
    let d2 = [ (0UL<rowindex>, 0UL<colindex>, 10); (2UL<rowindex>, 2UL<colindex>, 30) ]

    let f x y =
        match x, y with
        | Some a, Some b when a + b > 5 -> None
        | Some a, Some b -> Some(a + b)
        | Some a, None -> Some a
        | None, Some b -> Some b
        | None, None -> Some 0

    let expected =
        ListCOO(
            nrows,
            ncols,
            [ (0UL<rowindex>, 1UL<colindex>, 0)
              (0UL<rowindex>, 2UL<colindex>, 0)
              (0UL<rowindex>, 3UL<colindex>, 0)
              (1UL<rowindex>, 0UL<colindex>, 0)
              (1UL<rowindex>, 1UL<colindex>, 2)
              (1UL<rowindex>, 2UL<colindex>, 0)
              (1UL<rowindex>, 3UL<colindex>, 0)
              (2UL<rowindex>, 0UL<colindex>, 0)
              (2UL<rowindex>, 1UL<colindex>, 0)
              (2UL<rowindex>, 2UL<colindex>, 30)
              (2UL<rowindex>, 3UL<colindex>, 0)
              (3UL<rowindex>, 0UL<colindex>, 0)
              (3UL<rowindex>, 1UL<colindex>, 0)
              (3UL<rowindex>, 2UL<colindex>, 0)
              (3UL<rowindex>, 3UL<colindex>, 0) ]
        )

    let c1 = ListCOO(nrows, ncols, d1)
    let c2 = ListCOO(nrows, ncols, d2)

    let actual = cooMap2 c1 c2 f

    Assert.Equal(Ok expected, actual)

// === cooMapi tests ===

[<Fact>]
let ``cooMapi position-dependent values`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let data =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (1UL<rowindex>, 1UL<colindex>, 2)
          (2UL<rowindex>, 3UL<colindex>, 3) ]
        |> List.sort

    let coo = ListCOO(nrows, ncols, data)

    let f i j v =
        v |> Option.map (fun v -> v + (int (uint64 i)))

    let expected =
        ListCOO(
            nrows,
            ncols,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (1UL<rowindex>, 1UL<colindex>, 3)
              (2UL<rowindex>, 3UL<colindex>, 5) ]
        )

    let actual = cooMapi coo f

    Assert.Equal(expected, actual)

[<Fact>]
let ``cooMapi filters None results`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let data =
        [ (0UL<rowindex>, 0UL<colindex>, 1)
          (0UL<rowindex>, 1UL<colindex>, 5)
          (1UL<rowindex>, 0UL<colindex>, 3) ]

    let coo = ListCOO(nrows, ncols, data)

    let f _i _j v =
        v |> Option.bind (fun v -> if v > 2 then Some(v * 10) else None)

    let expected =
        ListCOO(nrows, ncols, [ (0UL<rowindex>, 1UL<colindex>, 50); (1UL<rowindex>, 0UL<colindex>, 30) ])

    let actual = cooMapi coo f

    Assert.Equal(expected, actual)

[<Fact>]
let ``cooMapi empty input`` () =
    let coo = ListCOO(4UL<nrows>, 4UL<ncols>, [])
    let f _i _j v = v |> Option.map (fun v -> v * 2)
    let actual = cooMapi coo f
    let expected = ListCOO(4UL<nrows>, 4UL<ncols>, [])
    Assert.Equal(expected, actual)

[<Fact>]
let ``cooMapi position-dependent fill of missing cells`` () =
    let nrows = 2UL<nrows>
    let ncols = 2UL<ncols>

    let data = [ (0UL<rowindex>, 0UL<colindex>, 7) ]
    let coo = ListCOO(nrows, ncols, data)

    let f i j v =
        match v with
        | Some x -> Some x
        | None -> Some(int (uint64 i + uint64 j))

    let actual = cooMapi coo f

    let expected =
        [ (0UL<rowindex>, 0UL<colindex>, 7)
          (0UL<rowindex>, 1UL<colindex>, 1)
          (1UL<rowindex>, 0UL<colindex>, 1)
          (1UL<rowindex>, 1UL<colindex>, 2) ]

    Assert.Equal<COOEntry<int> list>(expected, actual.entries)

// === cooMap2i tests ===

[<Fact>]
let ``cooMap2i position-dependent addition`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 = [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 3) ]
    let d2 = [ (0UL<rowindex>, 0UL<colindex>, 10); (2UL<rowindex>, 2UL<colindex>, 30) ]

    let f i j x y =
        match x, y with
        | Some a, Some b -> Some(a + b + (int (uint64 i)))
        | Some a, None -> Some a
        | None, Some b -> Some b
        | _ -> None

    let expected =
        ListCOO(nrows, ncols, [ (0UL<rowindex>, 0UL<colindex>, 11); (2UL<rowindex>, 2UL<colindex>, 35) ])

    let c1 = ListCOO(nrows, ncols, d1)
    let c2 = ListCOO(nrows, ncols, d2)
    let actual = cooMap2i c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooMap2i mismatched positions with index`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 = [ (0UL<rowindex>, 0UL<colindex>, 1); (2UL<rowindex>, 2UL<colindex>, 3) ]
    let d2 = [ (1UL<rowindex>, 1UL<colindex>, 10); (3UL<rowindex>, 3UL<colindex>, 30) ]

    let f i j x y =
        match x, y with
        | Some a, Some b -> Some(a + b)
        | Some a, None -> Some(a + (int (uint64 j)))
        | None, Some b -> Some(b + (int (uint64 i)))
        | _ -> None

    let expected =
        ListCOO(
            nrows,
            ncols,
            [ (0UL<rowindex>, 0UL<colindex>, 1)
              (1UL<rowindex>, 1UL<colindex>, 11)
              (2UL<rowindex>, 2UL<colindex>, 5)
              (3UL<rowindex>, 3UL<colindex>, 33) ]
        )

    let c1 = ListCOO(nrows, ncols, d1)
    let c2 = ListCOO(nrows, ncols, d2)
    let actual = cooMap2i c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooMap2i filters None results`` () =
    let nrows = 4UL<nrows>
    let ncols = 4UL<ncols>

    let d1 = [ (0UL<rowindex>, 0UL<colindex>, 1); (1UL<rowindex>, 1UL<colindex>, 2) ]
    let d2 = [ (0UL<rowindex>, 0UL<colindex>, 2); (2UL<rowindex>, 2UL<colindex>, 30) ]

    let f i j x y =
        match x, y with
        | Some a, Some b when a + b > 5 -> None
        | Some a, Some b -> Some(a + b)
        | _ -> None

    let expected = ListCOO(nrows, ncols, [ (0UL<rowindex>, 0UL<colindex>, 3) ])

    let c1 = ListCOO(nrows, ncols, d1)
    let c2 = ListCOO(nrows, ncols, d2)
    let actual = cooMap2i c1 c2 f

    Assert.Equal(Ok expected, actual)

[<Fact>]
let ``cooMap2i empty inputs`` () =
    let c1 = ListCOO(4UL<nrows>, 4UL<ncols>, [])
    let c2 = ListCOO(4UL<nrows>, 4UL<ncols>, [])
    let f _i _j x y = None
    let actual = cooMap2i c1 c2 f
    let expected = ListCOO(4UL<nrows>, 4UL<ncols>, [])
    Assert.Equal(Ok expected, actual)

// === mxmcoo tests ===

