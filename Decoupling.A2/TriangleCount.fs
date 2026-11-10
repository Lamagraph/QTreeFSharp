/// Triangle counting for approach A2. The mixed element types (graph 'a, counts uint64) live
/// directly in the algorithm's record instead of being split into separate records.
module A2.TriangleCount

open Result

type Error<'laErr, 'mErr> =
    | MXMProblem of 'laErr
    | MaskingProblem of 'mErr

    override this.ToString() =
        match this with
        | MXMProblem error -> sprintf "Matrix multiplication problem: %O" error
        | MaskingProblem error -> sprintf "Masking problem: %O" error

/// Everything TriangleCount needs: 'matA is the graph (elements 'a), 'matC the counts (uint64).
type Backend<'a, 'matA, 'matC, 'laErr, 'mErr> =
    { transpose: 'matA -> 'matA
      lowerTriangle: 'matA -> 'matA
      mxm:
          (uint64 option -> uint64 option -> uint64 option)
              -> ('a option -> 'a option -> uint64 option)
              -> 'matA
              -> 'matA
              -> Result<'matC, 'laErr>
      mask: 'matC -> 'matA -> ('a option -> bool) -> Result<'matC, 'mErr>
      foldAssociative: (uint64 option -> uint64 option -> uint64 option) -> uint64 option -> 'matC -> uint64 option }

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
    (backend: Backend<'a, 'matA, 'matC, 'laErr, 'mErr>)
    (graph: 'matA)
    : Result<uint64 option, Error<'laErr, 'mErr>> =
    let lower = backend.lowerTriangle graph

    resultM {
        let! paths =
            backend.mxm op_add op_mult lower (backend.transpose lower)
            |> Result.mapError MXMProblem

        let! triangles = backend.mask paths lower Option.isSome |> Result.mapError MaskingProblem
        return backend.foldAssociative op_add None triangles
    }
