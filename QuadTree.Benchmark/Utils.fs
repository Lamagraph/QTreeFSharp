module QuadTree.Benchmarks.Utils

open OptionMonoid

open System.IO
open BenchmarkDotNet.Configs

type MyConfig() =
    inherit ManualConfig()

let DIR_WITH_MATRICES = "../../../../../../../data/"

let readMtxRaw path directed =
    let getCooList (linewords: seq<string array>) =
        linewords
        |> Seq.map (fun x ->
            ((uint64 x.[0]) - 1UL), ((uint64 x.[1]) - 1UL), (if x.Length = 2 then 1.0 else double x.[2]))
        |> Seq.collect (fun (i, j, v) ->
            if not directed then
                [ (i * 1UL<Matrix.rowindex>, j * 1UL<Matrix.colindex>, v)
                  (j * 1UL<Matrix.rowindex>, i * 1UL<Matrix.colindex>, v) ]
            else
                [ (i * 1UL<Matrix.rowindex>, j * 1UL<Matrix.colindex>, v) ])
        |> List.ofSeq

    let lines = File.ReadLines(path)
    let removedComments = lines |> Seq.skipWhile (fun s -> s.[0] = '%')
    let linewords = removedComments |> Seq.map (fun s -> s.Split [| ' ' |])
    let first = Seq.head linewords

    let nrows, ncols, nnz = uint64 first.[0], uint64 first.[1], int first.[2]

    let tl = Seq.tail linewords

    let lst = getCooList tl

    if (directed && nnz <> lst.Length) || ((not directed) && nnz * 2 <> lst.Length) then
        failwithf
            "Incorrect matrix reading. Path: %A expected nnz: %A actual nnz: %A"
            path
            (if directed then nnz else nnz * 2)
            lst.Length

    let coo =
        Matrix.CoordinateList(nrows * 1UL<Matrix.nrows>, ncols * 1UL<Matrix.ncols>, lst)

    let qt = Matrix.fromCoordinateList coo

    (coo, qt)

let readMtx path directed = readMtxRaw path directed |> snd



let generateMatrix (size: int) (density: float) (rng: System.Random) =
    let coords =
        [ for i in 0 .. size - 1 do
              for j in 0 .. size - 1 do
                  if rng.NextDouble() < density then
                      let value = double (rng.Next(1, 4))
                      yield (uint64 i * 1UL<Matrix.rowindex>, uint64 j * 1UL<Matrix.colindex>, value) ]

    match
        Matrix.fromCoordinateList (
            Matrix.CoordinateList(uint64 size * 1UL<Matrix.nrows>, uint64 size * 1UL<Matrix.ncols>, coords)
        )
    with
    | Ok m -> m
    | Error msg -> failwithf "Failed to create matrix: %s" msg

let inline sumLookups n (lookupCoords: (uint64<Matrix.rowindex> * uint64<Matrix.colindex>) array) getFunc =
    let mutable acc = 0.0

    for k = 0 to n - 1 do
        let (i, j) = lookupCoords.[k]

        match getFunc i j with
        | Ok(Some v) -> acc <- acc + v
        | _ -> ()

    acc

let inline updateLookups
    n
    (lookupCoords: (uint64<Matrix.rowindex> * uint64<Matrix.colindex>) array)
    (lookupValues: double array)
    initial_m
    updateFunc
    =
    let mutable m = initial_m

    for k = 0 to n - 1 do
        let (i, j) = lookupCoords.[k]

        match updateFunc m i j (lookupValues.[k] * 2.0) with
        | Ok updated -> m <- updated
        | _ -> ()

    m

let inline map2iLogic i j (a: double option) (b: double option) : double option =
    match a, b with
    | Some x, Some y -> Some(x + y + float (uint64 i))
    | Some x, None -> Some x
    | None, Some y -> Some y
    | None, None -> None
