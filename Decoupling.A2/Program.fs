/// Usage examples for approach A2 (one record per algorithm).
module A2.Program

open Result
open Samples

let private runExamples
    (graph: Matrix.SparseMatrix<float>)
    (triangles: Matrix.SparseMatrix<unit>)
    (start: Vector.SparseVector<uint64>)
    (shortStart: Vector.SparseVector<uint64>)
    =
    printfn "1. Quad-tree entry points (same call shape as Graph.*):"

    QuadTree.bfsLevel graph start
    |> showResult showVector
    |> report "QuadTree.bfsLevel graph start"

    QuadTree.bfsParent graph start
    |> showResult showVector
    |> report "QuadTree.bfsParent graph start"

    QuadTree.sssp graph 0UL<Vector.index>
    |> showResult showVector
    |> report "QuadTree.sssp graph 0"

    QuadTree.triangleCount triangles
    |> showResult showCount
    |> report "QuadTree.triangleCount triangles"

    printfn "2. Generic functions with the algorithm's backend record:"

    BFS.bfsLevelWith QuadTreeBackend.bfs graph start
    |> showResult showVector
    |> report "BFS.bfsLevelWith QuadTreeBackend.bfs"

    SSSP.ssspWith QuadTreeBackend.sssp graph 0UL<Vector.index>
    |> showResult showVector
    |> report "SSSP.ssspWith QuadTreeBackend.sssp"

    TriangleCount.triangleCountWith QuadTreeBackend.triangleCount triangles
    |> showResult showCount
    |> report "TriangleCount.triangleCountWith QuadTreeBackend.triangleCount"

    printfn "3. Custom semiring through the generic bfsWith (shortest hop count, min instead of first):"

    let minHops x y =
        match x, y with
        | Some a, Some b -> Some(min a b)
        | Some a, None
        | None, Some a -> Some a
        | None, None -> None

    let toZero _ v = v |> Option.map (fun _ -> 0UL)

    BFS.bfsWith
        QuadTreeBackend.bfs
        minHops
        (fun (_, hops) _ -> Some(hops + 1UL))
        toZero
        graph
        (Vector.mapi start toZero)
    |> showResult showVector
    |> report "BFS.bfsWith QuadTreeBackend.bfs minHops ..."

    printfn "4. Replacing a field with record copy-and-update (a kernel that logs each BFS iteration):"

    let traced =
        { QuadTreeBackend.bfs with
            vxmiValues =
                fun op_add op_mult frontier matrix ->
                    printfn "     vxmi_values: frontier has %d values" (uint64 frontier.nvals)
                    LinearAlgebra.vxmi_values op_add op_mult frontier matrix }

    BFS.bfsLevelWith traced graph start
    |> showResult showVector
    |> report "BFS.bfsLevelWith { QuadTreeBackend.bfs with ... }"

    printfn "5. Errors are generic: BFS.Error<Vector.Error, LinearAlgebra.Error> for the quad-tree:"

    let explain (result: Result<_, BFS.Error<Vector.Error, LinearAlgebra.Error>>) =
        match result with
        | Error(BFS.NewFrontierCalculationProblem LinearAlgebra.Error.InconsistentSizeOfArguments) ->
            "kernel rejected the sizes (matched on the inner LinearAlgebra.Error case)"
        | other -> showResult showVector other

    QuadTree.bfsLevel graph shortStart
    |> explain
    |> report "QuadTree.bfsLevel graph (start of length 3)"

    QuadTree.bfsLevel graph shortStart
    |> showResult showVector
    |> report "same, via ToString()"

[<EntryPoint>]
let main _ =
    let samples =
        resultM {
            let! graph = matrix smallGraphSize smallGraph |> Result.mapError string
            let! triangles = matrix triangleGraphSize triangleGraph |> Result.mapError string
            let! start = startVector smallGraphSize 0UL 1UL |> Result.mapError string
            let! shortStart = startVector 3UL 0UL 1UL |> Result.mapError string
            return graph, triangles, start, shortStart
        }

    match samples with
    | Ok(graph, triangles, start, shortStart) ->
        printfn "Approach A2: one record per algorithm"
        runExamples graph triangles start shortStart
        0
    | Error error ->
        eprintfn "Invalid samples: %s" error
        1
