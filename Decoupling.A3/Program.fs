/// Usage examples for approach A3 (one combined record).
module A3.Program

open Result
open Samples

let private runExamples
    (graph: Matrix.SparseMatrix<float>)
    (triangles: Matrix.SparseMatrix<unit>)
    (start: Vector.SparseVector<uint64>)
    (shortStart: Vector.SparseVector<uint64>)
    (weightedTriangles: Matrix.SparseMatrix<float>)
    (triangleStart: Vector.SparseVector<uint64>)
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

    printfn "2. One backend value for every algorithm on one float graph (the triangle graph, unit weights):"

    // 'e = float, 'c = uint64 fixes all ten type parameters at once.
    let backend = QuadTreeBackend.backend<float, uint64>

    BFS.bfsLevelWith backend weightedTriangles triangleStart
    |> showResult showVector
    |> report "BFS.bfsLevelWith backend"

    SSSP.ssspWith backend weightedTriangles 0UL<Vector.index>
    |> showResult showVector
    |> report "SSSP.ssspWith backend"

    TriangleCount.triangleCountWith backend weightedTriangles
    |> showResult showCount
    |> report "TriangleCount.triangleCountWith backend"

    printfn "3. A different graph element type needs another instantiation of the whole record:"

    TriangleCount.triangleCountWith QuadTreeBackend.backend<unit, unit> triangles
    |> showResult showCount
    |> report "TriangleCount.triangleCountWith backend<unit, unit>"

    printfn "4. Replacing a field with record copy-and-update (a kernel that logs each BFS iteration):"

    let traced =
        { backend with
            vxmiValues =
                fun op_add op_mult frontier matrix ->
                    printfn "     vxmi_values: frontier has %d values" (uint64 frontier.nvals)
                    LinearAlgebra.vxmi_values op_add op_mult frontier matrix }

    BFS.bfsLevelWith traced graph start
    |> showResult showVector
    |> report "BFS.bfsLevelWith { backend with ... }"

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

            let! weightedTriangles =
                matrix triangleGraphSize (triangleGraph |> List.map (fun (r, c, ()) -> r, c, 1.0))
                |> Result.mapError string

            let! triangleStart = startVector triangleGraphSize 0UL 1UL |> Result.mapError string
            return graph, triangles, start, shortStart, weightedTriangles, triangleStart
        }

    match samples with
    | Ok(graph, triangles, start, shortStart, weightedTriangles, triangleStart) ->
        printfn "Approach A3: one combined record"
        runExamples graph triangles start shortStart weightedTriangles triangleStart
        0
    | Error error ->
        eprintfn "Invalid samples: %s" error
        1
