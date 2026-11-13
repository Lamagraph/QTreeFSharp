/// Generic triangle counting, independent of the matrix representation.
/// Representation-specific operations are supplied via `TriangleCountOps`; concrete
/// specializations live under `Graph.Impl.*` (e.g. `Graph.Impl.QuadTree.TriangleCount`).
module Graph.TriangleCount

open Result

type Error<'laErr, 'mErr> =
    | MXMProblem of 'laErr
    | MaskingProblem of 'mErr

/// Everything TriangleCount requires from a representation.
/// `'edge` is the graph's value type, `'graphMat` the graph matrix type,
/// `'countMat` the `uint64` count matrix type, `'laErr`/`'mErr` the errors of `mxm`/`mask`.
type TriangleCountOps<'edge, 'graphMat, 'countMat, 'laErr, 'mErr> =
    { lowerTriangle: 'graphMat -> 'graphMat
      transpose: 'graphMat -> 'graphMat
      mxm:
          (uint64 option -> uint64 option -> uint64 option)
              -> ('edge option -> 'edge option -> uint64 option)
              -> 'graphMat
              -> 'graphMat
              -> Result<'countMat, 'laErr>
      mask: 'countMat -> 'graphMat -> ('edge option -> bool) -> Result<'countMat, 'mErr>
      foldAssociative: (uint64 option -> uint64 option -> uint64 option) -> uint64 option -> 'countMat -> uint64 option }

let triangleCount
    (ops: TriangleCountOps<'edge, 'graphMat, 'countMat, 'laErr, 'mErr>)
    (graph: 'graphMat)
    : Result<uint64 option, Error<'laErr, 'mErr>> =
    let graph = ops.lowerTriangle graph

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

    resultM {
        let! C = ops.mxm op_add op_mult graph (ops.transpose graph) |> Result.mapError MXMProblem

        let! CMasked = ops.mask C graph Option.isSome |> Result.mapError MaskingProblem

        return ops.foldAssociative op_add None CMasked
    }
