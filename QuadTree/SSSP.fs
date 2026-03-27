module Graph.SSSP

open Common

type Error =
    | NewFrontierCalculationProblem of LinearAlgebra.Error
    | FrontierCalculationProblem of Vector.Error
    | VisitedCalculationProblem of Vector.Error

let mapError (err: LinearAlgebra.Error) = NewFrontierCalculationProblem err
let mapError' (err: Vector.Error) = FrontierCalculationProblem err
let mapError'' (err: Vector.Error) = VisitedCalculationProblem err

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

    let rec inner (frontier: Vector.SparseVector<_>) (visited: Vector.SparseVector<_>) iter_num =
        if frontier.nvals > 0UL<nvals> && iter_num <= int frontier.length then
            result {
                let! new_frontier =
                    LinearAlgebra.vxm op_add op_mult frontier graph
                    |> Common.Result.mapError mapError

                let op_min x y =
                    match (x, y) with
                    | Some v, Some u -> if v < u then Some v else None
                    | Some v, _ -> Some v
                    | _ -> None

                let! frontier =
                    Vector.map2 new_frontier visited op_min
                    |> Common.Result.mapError mapError'

                let! visited =
                    Vector.map2 visited frontier op_add
                    |> Common.Result.mapError mapError''

                return! inner frontier visited (iter_num + 1)
            }
        else
            Ok visited

    let frontier =
        Vector.CoordinateList((uint64 graph.ncols) * 1UL<Vector.dataLength>, [ startVertex * 1UL<Vector.index>, 0.0 ])
        |> Vector.fromCoordinateList

    inner frontier frontier 0
