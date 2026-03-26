module Graph.Boruvka.Tests

open System
open Xunit

open Matrix
open Vector
open Common

let checkResult name actual expected =
    match actual with 
    | Ok tree -> 
        let tree_transposed = Matrix.transpose tree
        let actual = Matrix.map2 tree tree_transposed (fun x y -> match (x,y) with | (Some(x),_) | (_, Some x) -> Some x | _ -> None)
        Assert.Equal(expected, actual)
    | x -> Assert.Fail (sprintf "Boruvka failed: %A" x)

[<Fact>]
let ``Boruvka MST 2 nodes.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(2UL<nrows>, 2UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 5UL
                1UL<rowindex>, 0UL<colindex>, 5UL
                ])
        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Matrix.CoordinateList(2UL<nrows>, 2UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 5UL
                1UL<rowindex>, 0UL<colindex>, 5UL
                ])
        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected


[<Fact>]
let ``Boruvka MST 3 nodes line.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(3UL<nrows>, 3UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL
                ])
        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Matrix.CoordinateList(3UL<nrows>, 3UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL
                ])
        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected



[<Fact>]
let ``Boruvka MST 4 nodes line.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(4UL<nrows>, 4UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL
                2UL<rowindex>, 3UL<colindex>, 3UL
                3UL<rowindex>, 2UL<colindex>, 3UL
                ])
        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Matrix.CoordinateList(4UL<nrows>, 4UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL
                2UL<rowindex>, 3UL<colindex>, 3UL
                3UL<rowindex>, 2UL<colindex>, 3UL
                ])
        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected


[<Fact>]
let ``Boruvka MST 5 nodes line.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(5UL<nrows>, 5UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL
                2UL<rowindex>, 3UL<colindex>, 3UL
                3UL<rowindex>, 2UL<colindex>, 3UL
                3UL<rowindex>, 4UL<colindex>, 4UL
                4UL<rowindex>, 3UL<colindex>, 4UL
                ])
        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Matrix.CoordinateList(5UL<nrows>, 5UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL
                2UL<rowindex>, 3UL<colindex>, 3UL
                3UL<rowindex>, 2UL<colindex>, 3UL
                3UL<rowindex>, 4UL<colindex>, 4UL
                4UL<rowindex>, 3UL<colindex>, 4UL
                ])
        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected


[<Fact>]
let ``Boruvka MST 5 nodes star.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(5UL<nrows>, 5UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 5UL
                1UL<rowindex>, 0UL<colindex>, 5UL
                0UL<rowindex>, 2UL<colindex>, 4UL
                2UL<rowindex>, 0UL<colindex>, 4UL
                0UL<rowindex>, 3UL<colindex>, 3UL
                3UL<rowindex>, 0UL<colindex>, 3UL
                0UL<rowindex>, 4UL<colindex>, 2UL
                4UL<rowindex>, 0UL<colindex>, 2UL
                ])
        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Matrix.CoordinateList(5UL<nrows>, 5UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 5UL
                1UL<rowindex>, 0UL<colindex>, 5UL
                0UL<rowindex>, 2UL<colindex>, 4UL
                2UL<rowindex>, 0UL<colindex>, 4UL
                0UL<rowindex>, 3UL<colindex>, 3UL
                3UL<rowindex>, 0UL<colindex>, 3UL
                0UL<rowindex>, 4UL<colindex>, 2UL
                4UL<rowindex>, 0UL<colindex>, 2UL
                ])
        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected


[<Fact>]
let ``Boruvka MST 5 nodes complete.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(5UL<nrows>, 5UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                0UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 0UL<colindex>, 2UL
                0UL<rowindex>, 3UL<colindex>, 3UL
                3UL<rowindex>, 0UL<colindex>, 3UL
                0UL<rowindex>, 4UL<colindex>, 4UL
                4UL<rowindex>, 0UL<colindex>, 4UL
                1UL<rowindex>, 2UL<colindex>, 5UL
                2UL<rowindex>, 1UL<colindex>, 5UL
                1UL<rowindex>, 3UL<colindex>, 6UL
                3UL<rowindex>, 1UL<colindex>, 6UL
                1UL<rowindex>, 4UL<colindex>, 7UL
                4UL<rowindex>, 1UL<colindex>, 7UL
                2UL<rowindex>, 3UL<colindex>, 8UL
                3UL<rowindex>, 2UL<colindex>, 8UL
                2UL<rowindex>, 4UL<colindex>, 9UL
                4UL<rowindex>, 2UL<colindex>, 9UL
                3UL<rowindex>, 4UL<colindex>, 10UL
                4UL<rowindex>, 3UL<colindex>, 10UL
                ])
        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Matrix.CoordinateList(5UL<nrows>, 5UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                0UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 0UL<colindex>, 2UL
                0UL<rowindex>, 3UL<colindex>, 3UL
                3UL<rowindex>, 0UL<colindex>, 3UL
                0UL<rowindex>, 4UL<colindex>, 4UL
                4UL<rowindex>, 0UL<colindex>, 4UL
                ])
        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected


[<Fact>]
let ``Boruvka MST two components.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(6UL<nrows>, 6UL<ncols>,[
                // Component 1: vertices 0,1,2
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL
                0UL<rowindex>, 2UL<colindex>, 3UL
                2UL<rowindex>, 0UL<colindex>, 3UL
                // Component 2: vertices 3,4,5
                3UL<rowindex>, 4UL<colindex>, 1UL
                4UL<rowindex>, 3UL<colindex>, 1UL
                4UL<rowindex>, 5UL<colindex>, 2UL
                5UL<rowindex>, 4UL<colindex>, 2UL
                3UL<rowindex>, 5UL<colindex>, 3UL
                5UL<rowindex>, 3UL<colindex>, 3UL
                ])
        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Matrix.CoordinateList(6UL<nrows>, 6UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL
                3UL<rowindex>, 4UL<colindex>, 1UL
                4UL<rowindex>, 3UL<colindex>, 1UL
                4UL<rowindex>, 5UL<colindex>, 2UL
                5UL<rowindex>, 4UL<colindex>, 2UL
                ])
        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected


[<Fact>]
let ``Boruvka MST cycle graph 6 nodes.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(6UL<nrows>, 6UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL
                2UL<rowindex>, 3UL<colindex>, 3UL
                3UL<rowindex>, 2UL<colindex>, 3UL
                3UL<rowindex>, 4UL<colindex>, 4UL
                4UL<rowindex>, 3UL<colindex>, 4UL
                4UL<rowindex>, 5UL<colindex>, 5UL
                5UL<rowindex>, 4UL<colindex>, 5UL
                5UL<rowindex>, 0UL<colindex>, 6UL
                0UL<rowindex>, 5UL<colindex>, 6UL
                ])
        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Matrix.CoordinateList(6UL<nrows>, 6UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL
                2UL<rowindex>, 3UL<colindex>, 3UL
                3UL<rowindex>, 2UL<colindex>, 3UL
                3UL<rowindex>, 4UL<colindex>, 4UL
                4UL<rowindex>, 3UL<colindex>, 4UL
                4UL<rowindex>, 5UL<colindex>, 5UL
                5UL<rowindex>, 4UL<colindex>, 5UL
                ])
        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected

///!!!!!!!!
[<Fact>]
let ``Boruvka MST complete bipartite K3,3.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(6UL<nrows>, 6UL<ncols>,[
                // K3,3: vertices 0,1,2 connected to 3,4,5
                0UL<rowindex>, 3UL<colindex>, 1UL
                3UL<rowindex>, 0UL<colindex>, 1UL

                0UL<rowindex>, 4UL<colindex>, 2UL
                4UL<rowindex>, 0UL<colindex>, 2UL
                
                0UL<rowindex>, 5UL<colindex>, 3UL
                5UL<rowindex>, 0UL<colindex>, 3UL
                
                1UL<rowindex>, 3UL<colindex>, 4UL
                3UL<rowindex>, 1UL<colindex>, 4UL
                
                1UL<rowindex>, 4UL<colindex>, 5UL
                4UL<rowindex>, 1UL<colindex>, 5UL
                
                1UL<rowindex>, 5UL<colindex>, 6UL
                5UL<rowindex>, 1UL<colindex>, 6UL
                
                2UL<rowindex>, 3UL<colindex>, 7UL
                3UL<rowindex>, 2UL<colindex>, 7UL
                
                2UL<rowindex>, 4UL<colindex>, 8UL
                4UL<rowindex>, 2UL<colindex>, 8UL
                
                2UL<rowindex>, 5UL<colindex>, 9UL
                5UL<rowindex>, 2UL<colindex>, 9UL
                ])
        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Matrix.CoordinateList(6UL<nrows>, 6UL<ncols>,[
                0UL<rowindex>, 3UL<colindex>, 1UL
                3UL<rowindex>, 0UL<colindex>, 1UL
                1UL<rowindex>, 3UL<colindex>, 4UL
                3UL<rowindex>, 1UL<colindex>, 4UL
                2UL<rowindex>, 3UL<colindex>, 7UL
                3UL<rowindex>, 2UL<colindex>, 7UL
                0UL<rowindex>, 4UL<colindex>, 2UL
                4UL<rowindex>, 0UL<colindex>, 2UL
                0UL<rowindex>, 5UL<colindex>, 3UL
                5UL<rowindex>, 0UL<colindex>, 3UL
                ])
        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected

[<Fact>]
let ``Boruvka MST random weights.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(8UL<nrows>, 8UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 7UL
                1UL<rowindex>, 0UL<colindex>, 7UL
                0UL<rowindex>, 2UL<colindex>, 5UL
                2UL<rowindex>, 0UL<colindex>, 5UL
                0UL<rowindex>, 3UL<colindex>, 9UL
                3UL<rowindex>, 0UL<colindex>, 9UL
                1UL<rowindex>, 2UL<colindex>, 3UL
                2UL<rowindex>, 1UL<colindex>, 3UL
                1UL<rowindex>, 3UL<colindex>, 4UL
                3UL<rowindex>, 1UL<colindex>, 4UL
                2UL<rowindex>, 3UL<colindex>, 2UL
                3UL<rowindex>, 2UL<colindex>, 2UL
                4UL<rowindex>, 5UL<colindex>, 1UL
                5UL<rowindex>, 4UL<colindex>, 1UL
                4UL<rowindex>, 6UL<colindex>, 6UL
                6UL<rowindex>, 4UL<colindex>, 6UL
                4UL<rowindex>, 7UL<colindex>, 8UL
                7UL<rowindex>, 4UL<colindex>, 8UL
                5UL<rowindex>, 6UL<colindex>, 3UL
                6UL<rowindex>, 5UL<colindex>, 3UL
                5UL<rowindex>, 7UL<colindex>, 5UL
                7UL<rowindex>, 5UL<colindex>, 5UL
                6UL<rowindex>, 7UL<colindex>, 2UL
                7UL<rowindex>, 6UL<colindex>, 2UL
                // Connect two components
                3UL<rowindex>, 4UL<colindex>, 10UL
                4UL<rowindex>, 3UL<colindex>, 10UL
                ])
        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Matrix.CoordinateList(8UL<nrows>, 8UL<ncols>,[
                0UL<rowindex>, 2UL<colindex>, 5UL
                2UL<rowindex>, 0UL<colindex>, 5UL
                1UL<rowindex>, 2UL<colindex>, 3UL
                2UL<rowindex>, 1UL<colindex>, 3UL
                2UL<rowindex>, 3UL<colindex>, 2UL
                3UL<rowindex>, 2UL<colindex>, 2UL
                1UL<rowindex>, 3UL<colindex>, 4UL
                3UL<rowindex>, 1UL<colindex>, 4UL
                4UL<rowindex>, 5UL<colindex>, 1UL
                5UL<rowindex>, 4UL<colindex>, 1UL
                5UL<rowindex>, 6UL<colindex>, 3UL
                6UL<rowindex>, 5UL<colindex>, 3UL
                6UL<rowindex>, 7UL<colindex>, 2UL
                7UL<rowindex>, 6UL<colindex>, 2UL
                3UL<rowindex>, 4UL<colindex>, 10UL
                4UL<rowindex>, 3UL<colindex>, 10UL
                ])
        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected

[<Fact>]
let ``Boruvka MST 8 nodes grid.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(8UL<nrows>, 8UL<ncols>,[
                // Row 0-1 connections
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL
                2UL<rowindex>, 3UL<colindex>, 1UL
                3UL<rowindex>, 2UL<colindex>, 1UL
                // Row 2-3 connections  
                0UL<rowindex>, 4UL<colindex>, 3UL
                4UL<rowindex>, 0UL<colindex>, 3UL
                1UL<rowindex>, 5UL<colindex>, 4UL
                5UL<rowindex>, 1UL<colindex>, 4UL
                2UL<rowindex>, 6UL<colindex>, 5UL
                6UL<rowindex>, 2UL<colindex>, 5UL
                3UL<rowindex>, 7UL<colindex>, 6UL
                7UL<rowindex>, 3UL<colindex>, 6UL
                // Cross row connections
                4UL<rowindex>, 5UL<colindex>, 2UL
                5UL<rowindex>, 4UL<colindex>, 2UL
                5UL<rowindex>, 6UL<colindex>, 1UL
                6UL<rowindex>, 5UL<colindex>, 1UL
                6UL<rowindex>, 7UL<colindex>, 3UL
                7UL<rowindex>, 6UL<colindex>, 3UL
                ])
        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Matrix.CoordinateList(8UL<nrows>, 8UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL
                2UL<rowindex>, 3UL<colindex>, 1UL
                3UL<rowindex>, 2UL<colindex>, 1UL
                5UL<rowindex>, 6UL<colindex>, 1UL
                6UL<rowindex>, 5UL<colindex>, 1UL
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL
                4UL<rowindex>, 5UL<colindex>, 2UL
                5UL<rowindex>, 4UL<colindex>, 2UL
                0UL<rowindex>, 4UL<colindex>, 3UL
                4UL<rowindex>, 0UL<colindex>, 3UL
                6UL<rowindex>, 7UL<colindex>, 3UL
                7UL<rowindex>, 6UL<colindex>, 3UL
                ])
        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected

[<Fact>]
let ``Boruvka MST 10 nodes random.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(10UL<nrows>, 10UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 4UL
                1UL<rowindex>, 0UL<colindex>, 4UL
                0UL<rowindex>, 5UL<colindex>, 2UL
                5UL<rowindex>, 0UL<colindex>, 2UL
                1UL<rowindex>, 2UL<colindex>, 3UL
                2UL<rowindex>, 1UL<colindex>, 3UL
                1UL<rowindex>, 6UL<colindex>, 5UL
                6UL<rowindex>, 1UL<colindex>, 5UL
                2UL<rowindex>, 3UL<colindex>, 1UL
                3UL<rowindex>, 2UL<colindex>, 1UL
                2UL<rowindex>, 7UL<colindex>, 4UL
                7UL<rowindex>, 2UL<colindex>, 4UL
                3UL<rowindex>, 4UL<colindex>, 2UL
                4UL<rowindex>, 3UL<colindex>, 2UL
                3UL<rowindex>, 8UL<colindex>, 6UL
                8UL<rowindex>, 3UL<colindex>, 6UL
                4UL<rowindex>, 9UL<colindex>, 3UL
                9UL<rowindex>, 4UL<colindex>, 3UL
                5UL<rowindex>, 6UL<colindex>, 1UL
                6UL<rowindex>, 5UL<colindex>, 1UL
                6UL<rowindex>, 7UL<colindex>, 2UL
                7UL<rowindex>, 6UL<colindex>, 2UL
                7UL<rowindex>, 8UL<colindex>, 1UL
                8UL<rowindex>, 7UL<colindex>, 1UL
                8UL<rowindex>, 9UL<colindex>, 4UL
                9UL<rowindex>, 8UL<colindex>, 4UL
                ])
        Matrix.fromCoordinateList clist

    let expected =
        let clist =
            Matrix.CoordinateList(10UL<nrows>, 10UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 4UL
                1UL<rowindex>, 0UL<colindex>, 4UL
                0UL<rowindex>, 5UL<colindex>, 2UL
                5UL<rowindex>, 0UL<colindex>, 2UL
                1UL<rowindex>, 2UL<colindex>, 3UL
                2UL<rowindex>, 1UL<colindex>, 3UL
                1UL<rowindex>, 6UL<colindex>, 5UL
                6UL<rowindex>, 1UL<colindex>, 5UL
                2UL<rowindex>, 3UL<colindex>, 1UL
                3UL<rowindex>, 2UL<colindex>, 1UL
                2UL<rowindex>, 7UL<colindex>, 4UL
                7UL<rowindex>, 2UL<colindex>, 4UL
                3UL<rowindex>, 4UL<colindex>, 2UL
                4UL<rowindex>, 3UL<colindex>, 2UL
                3UL<rowindex>, 8UL<colindex>, 6UL
                8UL<rowindex>, 3UL<colindex>, 6UL
                4UL<rowindex>, 9UL<colindex>, 3UL
                9UL<rowindex>, 4UL<colindex>, 3UL
                5UL<rowindex>, 6UL<colindex>, 1UL
                6UL<rowindex>, 5UL<colindex>, 1UL
                6UL<rowindex>, 7UL<colindex>, 2UL
                7UL<rowindex>, 6UL<colindex>, 2UL
                7UL<rowindex>, 8UL<colindex>, 1UL
                8UL<rowindex>, 7UL<colindex>, 1UL
                8UL<rowindex>, 9UL<colindex>, 4UL
                9UL<rowindex>, 8UL<colindex>, 4UL
                ])
        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected


[<Fact>]
let ``Boruvka MST simple triangle.`` () =
    printfn "!!! TEST STARTING !!!"
    let graph =
        let clist =
            Matrix.CoordinateList(3UL<nrows>, 3UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL

                0UL<rowindex>, 2UL<colindex>, 1UL 
                2UL<rowindex>, 0UL<colindex>, 1UL 

                1UL<rowindex>, 2UL<colindex>, 1UL 
                2UL<rowindex>, 1UL<colindex>, 1UL 

                ])

        Matrix.fromCoordinateList clist

    
    let expected =
        let clist =
            Matrix.CoordinateList(3UL<nrows>, 3UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL

                0UL<rowindex>, 2UL<colindex>, 1UL 
                2UL<rowindex>, 0UL<colindex>, 1UL 
                ])

        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected


[<Fact>]
let ``Boruvka MST simple square.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(4UL<nrows>, 4UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL

                1UL<rowindex>, 2UL<colindex>, 1UL 
                2UL<rowindex>, 1UL<colindex>, 1UL 

                2UL<rowindex>, 3UL<colindex>, 1UL 
                3UL<rowindex>, 2UL<colindex>, 1UL 

                0UL<rowindex>, 3UL<colindex>, 1UL 
                3UL<rowindex>, 0UL<colindex>, 1UL 

                ])

        Matrix.fromCoordinateList clist

    
    let expected =
        let clist =
            Matrix.CoordinateList(4UL<nrows>, 4UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL

                0UL<rowindex>, 3UL<colindex>, 1UL 
                3UL<rowindex>, 0UL<colindex>, 1UL 

                1UL<rowindex>, 2UL<colindex>, 1UL 
                2UL<rowindex>, 1UL<colindex>, 1UL 
                ])

        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected




[<Fact>]
let ``Boruvka MST simple square in two steps.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(4UL<nrows>, 4UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 2UL
                1UL<rowindex>, 0UL<colindex>, 2UL

                1UL<rowindex>, 2UL<colindex>, 1UL 
                2UL<rowindex>, 1UL<colindex>, 1UL 

                2UL<rowindex>, 3UL<colindex>, 2UL 
                3UL<rowindex>, 2UL<colindex>, 2UL 

                0UL<rowindex>, 3UL<colindex>, 1UL 
                3UL<rowindex>, 0UL<colindex>, 1UL 

                ])

        Matrix.fromCoordinateList clist

    
    let expected =
        let clist =
            Matrix.CoordinateList(4UL<nrows>, 4UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 2UL
                1UL<rowindex>, 0UL<colindex>, 2UL

                0UL<rowindex>, 3UL<colindex>, 1UL 
                3UL<rowindex>, 0UL<colindex>, 1UL 

                1UL<rowindex>, 2UL<colindex>, 1UL 
                2UL<rowindex>, 1UL<colindex>, 1UL 
                ])

        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected




[<Fact>]
let ``Boruvka MST.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(7UL<nrows>, 7UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 7UL
                1UL<rowindex>, 0UL<colindex>, 7UL

                0UL<rowindex>, 4UL<colindex>, 4UL 
                4UL<rowindex>, 0UL<colindex>, 4UL 

                1UL<rowindex>, 2UL<colindex>, 11UL 
                2UL<rowindex>, 1UL<colindex>, 11UL 

                1UL<rowindex>, 3UL<colindex>, 10UL 
                3UL<rowindex>, 1UL<colindex>, 10UL 

                1UL<rowindex>, 4UL<colindex>, 9UL 
                4UL<rowindex>, 1UL<colindex>, 9UL 

                2UL<rowindex>, 3UL<colindex>, 5UL
                3UL<rowindex>, 2UL<colindex>, 5UL

                4UL<rowindex>, 3UL<colindex>, 15UL 
                3UL<rowindex>, 4UL<colindex>, 15UL 

                4UL<rowindex>, 5UL<colindex>, 6UL 
                5UL<rowindex>, 4UL<colindex>, 6UL 

                5UL<rowindex>, 3UL<colindex>, 12UL 
                3UL<rowindex>, 5UL<colindex>, 12UL 

                6UL<rowindex>, 3UL<colindex>, 8UL 
                3UL<rowindex>, 6UL<colindex>, 8UL

                5UL<rowindex>, 6UL<colindex>, 13UL 
                6UL<rowindex>, 5UL<colindex>, 13UL 
                ])

        Matrix.fromCoordinateList clist

    
    let expected =
        let clist =
            Matrix.CoordinateList(7UL<nrows>, 7UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 7UL
                1UL<rowindex>, 0UL<colindex>, 7UL

                0UL<rowindex>, 4UL<colindex>, 4UL 
                4UL<rowindex>, 0UL<colindex>, 4UL 

                1UL<rowindex>, 3UL<colindex>, 10UL 
                3UL<rowindex>, 1UL<colindex>, 10UL 

                2UL<rowindex>, 3UL<colindex>, 5UL
                3UL<rowindex>, 2UL<colindex>, 5UL

                4UL<rowindex>, 5UL<colindex>, 6UL 
                5UL<rowindex>, 4UL<colindex>, 6UL 

                6UL<rowindex>, 3UL<colindex>, 8UL 
                3UL<rowindex>, 6UL<colindex>, 8UL

                ])

        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected

    
[<Fact>]
let ``Boruvka MST big.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(12UL<nrows>, 12UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL

                1UL<rowindex>, 11UL<colindex>, 1UL
                11UL<rowindex>, 1UL<colindex>, 1UL

                0UL<rowindex>, 11UL<colindex>, 1UL
                11UL<rowindex>, 0UL<colindex>, 1UL
//=================================================
                2UL<rowindex>, 3UL<colindex>, 1UL
                3UL<rowindex>, 2UL<colindex>, 1UL

                3UL<rowindex>, 4UL<colindex>, 1UL
                4UL<rowindex>, 3UL<colindex>, 1UL

                2UL<rowindex>, 4UL<colindex>, 1UL
                4UL<rowindex>, 2UL<colindex>, 1UL
//=================================================
                5UL<rowindex>, 6UL<colindex>, 1UL
                6UL<rowindex>, 5UL<colindex>, 1UL

                6UL<rowindex>, 7UL<colindex>, 1UL
                7UL<rowindex>, 6UL<colindex>, 1UL

                5UL<rowindex>, 7UL<colindex>, 1UL
                7UL<rowindex>, 5UL<colindex>, 1UL
//=================================================
                8UL<rowindex>, 9UL<colindex>, 1UL
                9UL<rowindex>, 8UL<colindex>, 1UL

                9UL<rowindex>, 10UL<colindex>, 1UL
                10UL<rowindex>, 9UL<colindex>, 1UL

                8UL<rowindex>, 10UL<colindex>, 1UL
                10UL<rowindex>, 8UL<colindex>, 1UL
//================================================
//================================================
                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL

                11UL<rowindex>, 4UL<colindex>, 2UL
                4UL<rowindex>, 11UL<colindex>, 2UL

                10UL<rowindex>, 5UL<colindex>, 2UL
                5UL<rowindex>, 10UL<colindex>, 2UL

                8UL<rowindex>, 7UL<colindex>, 2UL
                7UL<rowindex>, 8UL<colindex>, 2UL
//================================================
//================================================
                10UL<rowindex>, 11UL<colindex>, 3UL
                11UL<rowindex>, 10UL<colindex>, 3UL

                5UL<rowindex>, 4UL<colindex>, 3UL
                4UL<rowindex>, 5UL<colindex>, 3UL

                ])

        Matrix.fromCoordinateList clist

    
    let expected =
        let clist =
            Matrix.CoordinateList(12UL<nrows>, 12UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL

                0UL<rowindex>, 11UL<colindex>, 1UL
                11UL<rowindex>, 0UL<colindex>, 1UL

                2UL<rowindex>, 3UL<colindex>, 1UL
                3UL<rowindex>, 2UL<colindex>, 1UL

                2UL<rowindex>, 4UL<colindex>, 1UL
                4UL<rowindex>, 2UL<colindex>, 1UL

                5UL<rowindex>, 6UL<colindex>, 1UL
                6UL<rowindex>, 5UL<colindex>, 1UL

                5UL<rowindex>, 7UL<colindex>, 1UL
                7UL<rowindex>, 5UL<colindex>, 1UL

                8UL<rowindex>, 9UL<colindex>, 1UL
                9UL<rowindex>, 8UL<colindex>, 1UL

                8UL<rowindex>, 10UL<colindex>, 1UL
                10UL<rowindex>, 8UL<colindex>, 1UL

                1UL<rowindex>, 2UL<colindex>, 2UL
                2UL<rowindex>, 1UL<colindex>, 2UL

                5UL<rowindex>, 10UL<colindex>, 2UL
                10UL<rowindex>, 5UL<colindex>, 2UL

                4UL<rowindex>, 5UL<colindex>, 3UL
                5UL<rowindex>, 4UL<colindex>, 3UL

                ])

        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected

    
[<Fact>]
let ``Boruvka MST complex line.`` () =
    
    let graph =
        let clist =
            Matrix.CoordinateList(10UL<nrows>, 10UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL

                1UL<rowindex>, 2UL<colindex>, 2UL 
                2UL<rowindex>, 1UL<colindex>, 2UL 

                2UL<rowindex>, 3UL<colindex>, 1UL 
                3UL<rowindex>, 2UL<colindex>, 1UL 

                3UL<rowindex>, 4UL<colindex>, 3UL 
                4UL<rowindex>, 3UL<colindex>, 3UL 

                4UL<rowindex>, 5UL<colindex>, 1UL 
                5UL<rowindex>, 4UL<colindex>, 1UL 

                5UL<rowindex>, 6UL<colindex>, 1UL
                6UL<rowindex>, 5UL<colindex>, 1UL

                6UL<rowindex>, 7UL<colindex>, 2UL 
                7UL<rowindex>, 6UL<colindex>, 2UL 

                7UL<rowindex>, 8UL<colindex>, 1UL 
                8UL<rowindex>, 7UL<colindex>, 1UL 

                8UL<rowindex>, 9UL<colindex>, 1UL 
                9UL<rowindex>, 8UL<colindex>, 1UL 

                
                ])

        Matrix.fromCoordinateList clist

    
    let expected =
        let clist =
            Matrix.CoordinateList(10UL<nrows>, 10UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL

                1UL<rowindex>, 2UL<colindex>, 2UL 
                2UL<rowindex>, 1UL<colindex>, 2UL 

                2UL<rowindex>, 3UL<colindex>, 1UL 
                3UL<rowindex>, 2UL<colindex>, 1UL 

                3UL<rowindex>, 4UL<colindex>, 3UL 
                4UL<rowindex>, 3UL<colindex>, 3UL 

                4UL<rowindex>, 5UL<colindex>, 1UL 
                5UL<rowindex>, 4UL<colindex>, 1UL 

                5UL<rowindex>, 6UL<colindex>, 1UL
                6UL<rowindex>, 5UL<colindex>, 1UL

                6UL<rowindex>, 7UL<colindex>, 2UL 
                7UL<rowindex>, 6UL<colindex>, 2UL 

                7UL<rowindex>, 8UL<colindex>, 1UL 
                8UL<rowindex>, 7UL<colindex>, 1UL 

                8UL<rowindex>, 9UL<colindex>, 1UL 
                9UL<rowindex>, 8UL<colindex>, 1UL 


                ])

        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected


[<Fact>]
let ``Boruvka MST complex line 2.`` () =
    
    let graph =
        let clist =
            Matrix.CoordinateList(10UL<nrows>, 10UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL

                1UL<rowindex>, 2UL<colindex>, 2UL 
                2UL<rowindex>, 1UL<colindex>, 2UL 

                2UL<rowindex>, 3UL<colindex>, 1UL 
                3UL<rowindex>, 2UL<colindex>, 1UL 

                3UL<rowindex>, 9UL<colindex>, 3UL 
                9UL<rowindex>, 3UL<colindex>, 3UL 

                9UL<rowindex>, 8UL<colindex>, 1UL 
                8UL<rowindex>, 9UL<colindex>, 1UL 

                8UL<rowindex>, 7UL<colindex>, 1UL
                7UL<rowindex>, 8UL<colindex>, 1UL

                6UL<rowindex>, 7UL<colindex>, 2UL 
                7UL<rowindex>, 6UL<colindex>, 2UL 

                5UL<rowindex>, 6UL<colindex>, 1UL 
                6UL<rowindex>, 5UL<colindex>, 1UL 

                5UL<rowindex>, 4UL<colindex>, 1UL 
                4UL<rowindex>, 5UL<colindex>, 1UL 

                
                ])

        Matrix.fromCoordinateList clist

    
    let expected =
        let clist =
            Matrix.CoordinateList(10UL<nrows>, 10UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL

                1UL<rowindex>, 2UL<colindex>, 2UL 
                2UL<rowindex>, 1UL<colindex>, 2UL 

                2UL<rowindex>, 3UL<colindex>, 1UL 
                3UL<rowindex>, 2UL<colindex>, 1UL 

                3UL<rowindex>, 9UL<colindex>, 3UL 
                9UL<rowindex>, 3UL<colindex>, 3UL 

                9UL<rowindex>, 8UL<colindex>, 1UL 
                8UL<rowindex>, 9UL<colindex>, 1UL 

                8UL<rowindex>, 7UL<colindex>, 1UL
                7UL<rowindex>, 8UL<colindex>, 1UL

                6UL<rowindex>, 7UL<colindex>, 2UL 
                7UL<rowindex>, 6UL<colindex>, 2UL 

                5UL<rowindex>, 6UL<colindex>, 1UL 
                6UL<rowindex>, 5UL<colindex>, 1UL 

                5UL<rowindex>, 4UL<colindex>, 1UL 
                4UL<rowindex>, 5UL<colindex>, 1UL 

                ])

        Matrix.fromCoordinateList clist
        |> Ok

    checkResult (Graph.Boruvka.mst graph) expected
