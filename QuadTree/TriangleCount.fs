module Graph.TriangleCount

open Common

type TriangleCountError<'value1, 'value2, 'value3> =
    | MXMError of LinearAlgebra.MXMError<'value1, 'value2, 'value3>
    | MaskingError of Matrix.Error<'value3, 'value2>

// Assume non-oriented graph adjacency matrix
// Some _ -> edge, None -> no edge
// Computes triangle count
let triangle_count (graph: Matrix.SparseMatrix<_>) =
    let graph = Matrix.getLowerTriangle graph

    let op_add o1 o2 =
        match o1, o2 with
        | Some x, Some y -> Some <| x + y
        | Some x, None
        | None, Some x -> Some x
        | None, None -> None

    let op_mult o1 o2 =
        match o1, o2 with
        | Some _, Some _ -> Some 1UL
        | _ -> None

    let C = LinearAlgebra.mxm op_add op_mult graph (Matrix.transpose graph)

    let CMasked =
        match C with
        | Result.Success matrix ->
            match Matrix.mask matrix graph Option.isSome with
            | Result.Success m -> Result.Success m
            | Result.Failure e -> Result.Failure <| TriangleCountError.MaskingError e
        | Result.Failure e -> Result.Failure <| TriangleCountError.MXMError e

    let result =
        match CMasked with
        | Result.Success matrix -> Result.Success(Matrix.fold op_add None matrix)
        | Result.Failure e -> Result.Failure e

    result


