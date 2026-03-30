module Graph.BFS

open Common
open Result

type Error =
    | NewFrontierCalculationProblem of LinearAlgebra.Error
    | FrontierCalculationProblem of Vector.Error
    | VisitedCalculationProblem of Vector.Error

let mapError (err: LinearAlgebra.Error) = NewFrontierCalculationProblem err
let mapError' (err: Vector.Error) = FrontierCalculationProblem err
let mapError'' (err: Vector.Error) = VisitedCalculationProblem err

let bfs_level graph startVertices =
    let rec inner level (frontier: Vector.SparseVector<_>) (visited: Vector.SparseVector<_>) =
        if frontier.nvals > 0UL<nvals> then
            resultM {
                let! new_frontier = 
                    LinearAlgebra.vxm 
                        (fun x y -> match (x, y) with | Some(v), _ | _, Some(v) -> Some(v) | _ -> None)
                        (fun x y -> match (x, y) with | Some(v), Some(_) -> Some(v) | _ -> None)
                        frontier graph
                    |> Result.mapError mapError

                let! frontier = 
                    Vector.mask new_frontier visited (fun x -> x.IsNone)
                    |> Result.mapError mapError'

                let! visited = 
                    Vector.map2 visited new_frontier (fun x y ->
                        match (x, y) with
                        | (Some(_), _) -> x
                        | (None, Some(_)) -> Some(level)
                        | _ -> None)
                    |> Result.mapError mapError''

                return! inner (level + 1UL) frontier visited
            }
        else
            Ok visited

    let initialVisited = Vector.map startVertices (Option.map (fun x -> 0UL))
    inner 1UL startVertices initialVisited
