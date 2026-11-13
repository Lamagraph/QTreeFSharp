/// Generic single-source shortest paths (Bellman-Ford style over `float` weights),
/// independent of the vector/matrix representation.
/// Representation-specific operations are supplied via `SsspOps`; concrete
/// specializations live under `Graph.Impl.*` (e.g. `Graph.Impl.QuadTree.SSSP`).
module Graph.SSSP

open Common
open Result

type Error<'vErr, 'laErr> =
    | NewFrontierCalculationProblem of 'laErr
    | FrontierCalculationProblem of 'vErr
    | VisitedCalculationProblem of 'vErr
    | FrontierInitProblem of 'vErr

/// Everything SSSP requires from a representation.
/// `'vec`/`'mat` are the concrete `float` vector/matrix types, `'vErr`/`'laErr` their errors.
type SsspOps<'vec, 'mat, 'vErr, 'laErr> =
    {
        nvals: 'vec -> uint64<nvals>
        length: 'vec -> uint64<Vector.dataLength>
        ncols: 'mat -> uint64<Matrix.ncols>
        /// Builds a vector of the given length from (index, value) pairs.
        fromCoordinates: uint64<Vector.dataLength> -> (uint64<Vector.index> * float) list -> Result<'vec, 'vErr>
        map2: 'vec -> 'vec -> (float option -> float option -> float option) -> Result<'vec, 'vErr>
        vxm:
            (float option -> float option -> float option)
                -> (float option -> float option -> float option)
                -> 'vec
                -> 'mat
                -> Result<'vec, 'laErr>
    }

let sssp
    (ops: SsspOps<'vec, 'mat, 'vErr, 'laErr>)
    (graph: 'mat)
    (startVertex: uint64)
    : Result<'vec, Error<'vErr, 'laErr>> =
    let op_add x y =
        match (x, y) with
        | Some(u), Some(v) -> Some(min u v)
        | Some(v), _
        | _, Some(v) -> Some(v)
        | _ -> None

    let op_mult x y =
        match (x, y) with
        | Some(u), Some(v) -> Some(u + v)
        | _ -> None

    let rec inner (frontier: 'vec) (visited: 'vec) iterNum =
        if ops.nvals frontier > 0UL<nvals> && iterNum <= int (ops.length frontier) then
            resultM {
                let! newFrontier =
                    ops.vxm op_add op_mult frontier graph
                    |> Result.mapError NewFrontierCalculationProblem

                let op_min x y =
                    match (x, y) with
                    | Some v, Some u -> if v < u then Some v else None
                    | Some v, _ -> Some v
                    | _ -> None

                let! frontier =
                    ops.map2 newFrontier visited op_min
                    |> Result.mapError FrontierCalculationProblem

                let! visited = ops.map2 visited frontier op_add |> Result.mapError VisitedCalculationProblem

                return! inner frontier visited (iterNum + 1)
            }
        else
            Ok visited

    resultM {
        let! frontier =
            ops.fromCoordinates
                ((uint64 (ops.ncols graph)) * 1UL<Vector.dataLength>)
                [ (startVertex * 1UL<Vector.index>, 0.0) ]
            |> Result.mapError FrontierInitProblem

        return! inner frontier frontier 0
    }
