module Graph.Boruvka.Tests

open System
open Xunit

open Matrix
open Vector
open Common


//[<Fact>]
let ``Boruvka MST simple triangle.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(7UL<nrows>, 7UL<ncols>,[
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
            Matrix.CoordinateList(7UL<nrows>, 7UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL

                0UL<rowindex>, 2UL<colindex>, 1UL 
                2UL<rowindex>, 0UL<colindex>, 1UL 
                ])

        Matrix.fromCoordinateList clist
        |> Result.Success

    //let actual = 
    match Graph.Boruvka.mst graph with 
    | Result.Success tree -> 
        let tree_transposed = Matrix.transpose tree
        let actual = Matrix.map2 tree tree_transposed (fun x y -> match (x,y) with | (Some(x),_) | (_, Some x) -> Some x | _ -> None)
        match actual with 
        | Result.Success actual -> Tests.printMatrixCoordinate actual
        | _ -> printfn "Failed"
        Assert.Equal(expected, actual)
        //actual
        //|> Result.Success
    | x -> Assert.Fail (sprintf "Boruvka failed: %A" x)


//[<Fact>]
let ``Boruvka MST simple square.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(7UL<nrows>, 7UL<ncols>,[
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
            Matrix.CoordinateList(7UL<nrows>, 7UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 1UL
                1UL<rowindex>, 0UL<colindex>, 1UL

                0UL<rowindex>, 3UL<colindex>, 1UL 
                3UL<rowindex>, 0UL<colindex>, 1UL 

                1UL<rowindex>, 2UL<colindex>, 1UL 
                2UL<rowindex>, 1UL<colindex>, 1UL 
                ])

        Matrix.fromCoordinateList clist
        |> Result.Success

    //let actual = 
    match Graph.Boruvka.mst graph with 
    | Result.Success tree -> 
        let tree_transposed = Matrix.transpose tree
        let actual = Matrix.map2 tree tree_transposed (fun x y -> match (x,y) with | (Some(x),_) | (_, Some x) -> Some x | _ -> None)
        match actual with 
        | Result.Success actual -> Tests.printMatrixCoordinate actual
        | _ -> printfn "Failed"
        Assert.Equal(expected, actual)
        //actual
        //|> Result.Success
    | x -> Assert.Fail (sprintf "Boruvka failed: %A" x)




//[<Fact>]
let ``Boruvka MST simple square in two steps.`` () =
    System.Console.Error.WriteLine("TEST STARTING")
    let graph =
        let clist =
            Matrix.CoordinateList(7UL<nrows>, 7UL<ncols>,[
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
            Matrix.CoordinateList(7UL<nrows>, 7UL<ncols>,[
                0UL<rowindex>, 1UL<colindex>, 2UL
                1UL<rowindex>, 0UL<colindex>, 2UL

                0UL<rowindex>, 3UL<colindex>, 1UL 
                3UL<rowindex>, 0UL<colindex>, 1UL 

                1UL<rowindex>, 2UL<colindex>, 1UL 
                2UL<rowindex>, 1UL<colindex>, 1UL 
                ])

        Matrix.fromCoordinateList clist
        |> Result.Success

    //let actual = 
    match Graph.Boruvka.mst graph with 
    | Result.Success tree -> 
        let tree_transposed = Matrix.transpose tree
        let actual = Matrix.map2 tree tree_transposed (fun x y -> match (x,y) with | (Some(x),_) | (_, Some x) -> Some x | _ -> None)
        match actual with 
        | Result.Success actual -> Tests.printMatrixCoordinate actual
        | _ -> printfn "Failed"
        Assert.Equal(expected, actual)
        //actual
        //|> Result.Success
    | x -> Assert.Fail (sprintf "Boruvka failed: %A" x)




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
        |> Result.Success

    //let actual = 
    match Graph.Boruvka.mst graph with 
    | Result.Success tree -> 
        let tree_transposed = Matrix.transpose tree
        let actual = Matrix.map2 tree tree_transposed (fun x y -> match (x,y) with | (Some(x),_) | (_, Some x) -> Some x | _ -> None)
        match actual with 
        | Result.Success actual -> Tests.printMatrixCoordinate actual
        | _ -> printfn "Failed"
        Assert.Equal(expected, actual)
    | x -> Assert.Fail (sprintf "Boruvka failed: %A" x)

    

