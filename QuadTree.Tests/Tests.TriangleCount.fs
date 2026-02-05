module Graph.TriangleCount.Tests

open System
open Xunit

open Common
open Matrix
open Graph.TriangleCount

[<Fact>]
let ``7V Triangle count`` () =

    //     0 1 2 3 4 5 6
    //
    // 0   0 1 0 1 0 0 0
    // 1   1 0 0 1 1 0 1
    // 2   0 0 0 1 0 1 1
    // 3   1 1 1 0 0 1 1
    // 4   0 1 0 0 0 1 1
    // 5   0 0 1 1 1 0 0
    // 6   0 1 1 1 1 0 0

    let g =
        let d =
            [ 1UL<rowindex>, 0UL<colindex>, ()
              3UL<rowindex>, 0UL<colindex>, ()
              3UL<rowindex>, 1UL<colindex>, ()
              3UL<rowindex>, 2UL<colindex>, ()
              4UL<rowindex>, 1UL<colindex>, ()
              5UL<rowindex>, 2UL<colindex>, ()
              5UL<rowindex>, 3UL<colindex>, ()
              5UL<rowindex>, 4UL<colindex>, ()
              6UL<rowindex>, 1UL<colindex>, ()
              6UL<rowindex>, 2UL<colindex>, ()
              6UL<rowindex>, 3UL<colindex>, ()
              6UL<rowindex>, 4UL<colindex>, ()

              0UL<rowindex>, 1UL<colindex>, ()
              0UL<rowindex>, 3UL<colindex>, ()
              1UL<rowindex>, 3UL<colindex>, ()
              2UL<rowindex>, 3UL<colindex>, ()
              1UL<rowindex>, 4UL<colindex>, ()
              2UL<rowindex>, 5UL<colindex>, ()
              3UL<rowindex>, 5UL<colindex>, ()
              4UL<rowindex>, 5UL<colindex>, ()
              1UL<rowindex>, 6UL<colindex>, ()
              2UL<rowindex>, 6UL<colindex>, ()
              3UL<rowindex>, 6UL<colindex>, ()
              4UL<rowindex>, 6UL<colindex>, () ]

        fromCoordinateList (CoordinateList(7UL<nrows>, 7UL<ncols>, d))

    let expected = 5UL

    let actual =
        match triangle_count g with
        | Result.Success(Some x) -> x
        | _ -> failwith "Unreachable"

    Assert.Equal(expected, actual)
