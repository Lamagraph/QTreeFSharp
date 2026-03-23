module Graph.Boruvka.Tests

open System
open Xunit

open Matrix
open Vector
open Common


[<Fact>]
let ``Boruvka MST.`` () =
    let graph =
        let clist =
            Matrix.CoordinateList(7UL<nrows>, 7UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 7
                1UL<rowindex>, 0UL<colindex>, 7

                0UL<rowindex>, 4UL<colindex>, 4 
                4UL<rowindex>, 0UL<colindex>, 4 

                1UL<rowindex>, 2UL<colindex>, 11 
                2UL<rowindex>, 1UL<colindex>, 11 

                1UL<rowindex>, 3UL<colindex>, 10 
                3UL<rowindex>, 1UL<colindex>, 10 

                1UL<rowindex>, 4UL<colindex>, 9 
                4UL<rowindex>, 1UL<colindex>, 9 

                2UL<rowindex>, 3UL<colindex>, 5
                3UL<rowindex>, 2UL<colindex>, 5

                4UL<rowindex>, 3UL<colindex>, 15 
                3UL<rowindex>, 4UL<colindex>, 15 

                4UL<rowindex>, 5UL<colindex>, 6 
                5UL<rowindex>, 4UL<colindex>, 6 

                5UL<rowindex>, 3UL<colindex>, 12 
                3UL<rowindex>, 5UL<colindex>, 12 

                6UL<rowindex>, 3UL<colindex>, 8 
                3UL<rowindex>, 6UL<colindex>, 8

                5UL<rowindex>, 6UL<colindex>, 13 
                6UL<rowindex>, 5UL<colindex>, 13 
                ])

        Matrix.fromCoordinateList clist

    
    let expected =
        let clist =
            Matrix.CoordinateList(7UL<nrows>, 7UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 7
                1UL<rowindex>, 0UL<colindex>, 7

                0UL<rowindex>, 4UL<colindex>, 4 
                4UL<rowindex>, 0UL<colindex>, 4 

                1UL<rowindex>, 3UL<colindex>, 10 
                3UL<rowindex>, 1UL<colindex>, 10 

                2UL<rowindex>, 3UL<colindex>, 5
                3UL<rowindex>, 2UL<colindex>, 5

                4UL<rowindex>, 5UL<colindex>, 6 
                5UL<rowindex>, 4UL<colindex>, 6 

                6UL<rowindex>, 3UL<colindex>, 8 
                3UL<rowindex>, 6UL<colindex>, 8

                ])

        Matrix.fromCoordinateList clist
        |> Result.Success

    let actual = Graph.Boruvka.mst graph

    Assert.Equal(expected, actual)

