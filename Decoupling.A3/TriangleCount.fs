/// Triangle counting for approach A3, using the `m*` and `mxm` fields of the full backend.
module A3.TriangleCount

open Result
open A3.Backend

type Error<'laErr, 'mErr> =
    | MXMProblem of 'laErr
    | MaskingProblem of 'mErr

    override this.ToString() =
        match this with
        | MXMProblem error -> sprintf "Matrix multiplication problem: %O" error
        | MaskingProblem error -> sprintf "Masking problem: %O" error

let private op_add x y =
    match x, y with
    | Some a, Some b -> Some(a + b)
    | Some a, None
    | None, Some a -> Some a
    | None, None -> None

let private op_mult x y =
    match x, y with
    | Some _, Some _ -> Some 1UL
    | _ -> None

let triangleCountWith
    (backend: Backend<'e, 'c, 'vecC, 'matE, 'matU, 'vecF, 'matF, 'vErr, 'mErr, 'laErr>)
    (graph: 'matE)
    : Result<uint64 option, Error<'laErr, 'mErr>> =
    let lower = backend.mLowerTriangle graph

    resultM {
        let! paths =
            backend.mxm op_add op_mult lower (backend.mTranspose lower)
            |> Result.mapError MXMProblem

        let! triangles = backend.mMask paths lower Option.isSome |> Result.mapError MaskingProblem
        return backend.mFold op_add None triangles
    }
