module Graph.BFS

open Common

type Error<'t1, 't2> =
    | NewFrontierCalculationProblem of LinearAlgebra.Error<'t1, 't2, 't1>
    | FrontierCalculationProblem of Vector.Error<'t1, 't1>
    | VisitedCalculationProblem of Vector.Error<'t1, 't1>

let bfs_level graph startVertices =
    let rec inner level (frontier: Vector.SparseVector<_>) (visited: Vector.SparseVector<_>) =
        if frontier.nvals > 0UL<nvals> then
            let op_add x y =
                match (x, y) with
                | Some(v), _
                | _, Some(v) -> Some(v)
                | _ -> None

            let op_mult x y =
                match (x, y) with
                | Some(v), Some(_) -> Some(v)
                | _ -> None

            let new_frontier = LinearAlgebra.vxm op_add op_mult frontier graph

            match new_frontier with
            | Result.Failure(e) -> Result.Failure(NewFrontierCalculationProblem(e))
            | Result.Success(new_frontier) ->
                let frontier = Vector.mask new_frontier visited (fun x -> x.IsNone)

                match frontier with
                | Result.Failure(e) -> Result.Failure(FrontierCalculationProblem(e))
                | Result.Success(frontier) ->
                    let op_add x y =
                        match (x, y) with
                        | (Some(_), _) -> x
                        | (None, Some(_)) -> Some(level)
                        | _ -> None

                    let visited = Vector.map2 visited new_frontier op_add

                    match visited with
                    | Result.Failure(e) -> Result.Failure(VisitedCalculationProblem(e))
                    | Result.Success(visited) -> inner (level + 1UL) frontier visited
        else
            Result.Success visited

    inner 1UL startVertices (Vector.map startVertices (Option.map (fun x -> 0UL)))
