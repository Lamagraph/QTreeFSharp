module Graph.TriangleCount

open Common

type TriangleCountError =
    | MXMError of LinearAlgebra.Error
    | MaskingError of Matrix.Error

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
        | Ok matrix ->
            match Matrix.mask matrix graph Option.isSome with
            | Ok m -> Ok m
            | Error e -> Error (TriangleCountError.MaskingError e)
        | Error e -> Error (TriangleCountError.MXMError e)

    let result =
        match CMasked with
        | Ok matrix -> Ok(Matrix.foldAssociative op_add None matrix)
        | Error e -> Error e

    result
