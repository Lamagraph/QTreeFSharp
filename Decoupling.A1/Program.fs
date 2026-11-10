/// Usage examples for approach A1 (small records per capability).
module A1.Program

open Result
open Samples
open A1.Backend

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

    printfn "2. Generic functions with every capability record passed explicitly:"

    BFS.bfsLevelWith QuadTreeBackend.vector QuadTreeBackend.vectorMask QuadTreeBackend.vxmiValues graph start
    |> showResult showVector
    |> report "BFS.bfsLevelWith vector vectorMask vxmiValues"

    TriangleCount.triangleCountWith
        QuadTreeBackend.matrix
        QuadTreeBackend.matrix
        QuadTreeBackend.mxm
        QuadTreeBackend.matrixMask
        triangles
    |> showResult showCount
    |> report "TriangleCount.triangleCountWith matrix matrix mxm mask"

    printfn "3. Section 7 bundles: one value per algorithm, built from the same records:"

    Bundled.bfsLevel Bundled.QuadTree.bfs graph start
    |> showResult showVector
    |> report "Bundled.bfsLevel QuadTree.bfs"

    Bundled.sssp Bundled.QuadTree.sssp graph 0UL<Vector.index>
    |> showResult showVector
    |> report "Bundled.sssp QuadTree.sssp"

    Bundled.triangleCount Bundled.QuadTree.triangleCount triangles
    |> showResult showCount
    |> report "Bundled.triangleCount QuadTree.triangleCount"

    printfn "4. Replacing a single capability (a kernel that logs each BFS iteration):"

    let tracedKernel: VxmiValuesOps<float, uint64, Vector.SparseVector<uint64>, Matrix.SparseMatrix<float>, _> =
        { vxmiValues =
            fun op_add op_mult frontier matrix ->
                printfn "     vxmi_values: frontier has %d values" (uint64 frontier.nvals)
                QuadTreeBackend.vxmiValues.vxmiValues op_add op_mult frontier matrix }

    BFS.bfsLevelWith QuadTreeBackend.vector QuadTreeBackend.vectorMask tracedKernel graph start
    |> showResult showVector
    |> report "BFS.bfsLevelWith vector vectorMask tracedKernel"

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
        printfn "Approach A1: small records per capability"
        runExamples graph triangles start shortStart
        0
    | Error error ->
        eprintfn "Invalid samples: %s" error
        1
