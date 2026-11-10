/// Triangle counting written against A1 capability records.
///
/// The graph matrix ('matA, elements 'a) and the count matrix ('matC, elements uint64) are
/// different containers, so the element-preserving `MatrixOps` is requested twice and the
/// mixed-type `mxm` and `mask` come from their own records.
module A1.TriangleCount

open Result
open A1.Backend

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
    (graphOps: MatrixOps<'a, 'matA>)
    (countOps: MatrixOps<uint64, 'matC>)
    (mxm: MxmOps<'a, 'a, uint64, 'matA, 'matA, 'matC, 'laErr>)
    (mask: MatrixMaskOps<'a, 'matC, 'matA, 'mErr>)
    (graph: 'matA)
    : Result<uint64 option, Error<'laErr, 'mErr>> =
    let lower = graphOps.lowerTriangle graph

    resultM {
        let! paths =
            mxm.mxm op_add op_mult lower (graphOps.transpose lower)
            |> Result.mapError MXMProblem

        let! triangles = mask.mask paths lower Option.isSome |> Result.mapError MaskingProblem
        return countOps.foldAssociative op_add None triangles
    }
