module COOList.Operations.Tests

open System
open Xunit
open Matrix
open COOList
open Common

[<Fact>]
let TestGet () =
    let coo = ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])
    let actual = cooGet (coo, 0UL<rowindex>, 0UL<colindex>)
    Assert.Equal(Ok(Some 1), actual)

[<Fact>]
let TestUpdate () =
    let coo = ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    let expected =
        ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 99) ])

    match cooUpdate (coo, 0UL<rowindex>, 0UL<colindex>, 99) with
    | Ok actual -> Assert.Equal<COOEntry<int> list>(expected.entries, actual.entries)
    | _ -> failwith "Failed"

[<Fact>]
let TestMap () =
    let coo = ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])

    let expected =
        ListCOO(4UL<nrows>, 4UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 2) ])

    let actual = cooMap coo (fun v -> v |> Option.map (fun x -> x * 2))
    Assert.Equal<COOEntry<int> list>(expected.entries, actual.entries)

[<Fact>]
let TestMap2 () =
    let m1 = ListCOO(2UL<nrows>, 2UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 1) ])
    let m2 = ListCOO(2UL<nrows>, 2UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 2) ])

    let expected =
        ListCOO(2UL<nrows>, 2UL<ncols>, [ (0UL<rowindex>, 0UL<colindex>, 3) ])

    let op x y =
        match x, y with
        | Some a, Some b -> Some(a + b)
        | Some a, None
        | None, Some a -> Some a
        | _ -> None

    match cooMap2 m1 m2 op with
    | Ok actual -> Assert.Equal<COOEntry<int> list>(expected.entries, actual.entries)
    | _ -> failwith "Failed"
