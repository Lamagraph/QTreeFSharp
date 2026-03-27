module Graph.TriangleCount

open Common

type Error =
    | MXMError of LinearAlgebra.Error
    | MaskingError of Matrix.Error

let mapError (err: LinearAlgebra.Error) = MXMError err
let mapError' (err: Matrix.Error) = MaskingError err

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

    result {
        let! C =
            LinearAlgebra.mxm op_add op_mult graph (Matrix.transpose graph)
            |> Common.Result.mapError mapError

        let! CMasked =
            Matrix.mask C graph Option.isSome
            |> Common.Result.mapError mapError'

        return Matrix.foldAssociative op_add None CMasked
    }
