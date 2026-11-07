module Graph.SSSP

open Common
open Result

type Error =
    | NewFrontierCalculationProblem of LinearAlgebra.Error
    | FrontierCalculationProblem of Vector.Error
    | VisitedCalculationProblem of Vector.Error
    | FrontierInitProblem of Vector.Error

let sssp graph (startVertex: uint64) =
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

    let rec inner (frontier: Vector.SparseVector<_>) (visited: Vector.SparseVector<_>) iterNum =
        if frontier.nvals > 0UL<nvals> && iterNum <= int frontier.length then
            resultM {
                let! newFrontier =
                    LinearAlgebra.vxm op_add op_mult frontier graph
                    |> Result.mapError NewFrontierCalculationProblem

                let op_min x y =
                    match (x, y) with
                    | Some v, Some u -> if v < u then Some v else None
                    | Some v, _ -> Some v
                    | _ -> None

                let! frontier =
                    Vector.map2 newFrontier visited op_min
                    |> Result.mapError FrontierCalculationProblem

                let! visited = Vector.map2 visited frontier op_add |> Result.mapError VisitedCalculationProblem

                return! inner frontier visited (iterNum + 1)
            }
        else
            Ok visited

    let frontierResult =
        Vector.CoordinateList((uint64 graph.ncols) * 1UL<Vector.dataLength>, [ startVertex * 1UL<Vector.index>, 0.0 ])
        |> Vector.fromCoordinateList

    resultM {
        let! frontier = frontierResult |> Result.mapError FrontierInitProblem
        return! inner frontier frontier 0
    }
