/// SSSP for approach A3, using the `f*` (float) fields of the full backend.
module A3.SSSP

open Common
open Result
open A3.Backend

type Error<'vErr, 'laErr> =
    | NewFrontierCalculationProblem of 'laErr
    | FrontierCalculationProblem of 'vErr
    | VisitedCalculationProblem of 'vErr
    | FrontierInitProblem of 'vErr

    override this.ToString() =
        match this with
        | NewFrontierCalculationProblem error -> sprintf "New frontier calculation problem: %O" error
        | FrontierCalculationProblem error -> sprintf "Frontier calculation problem: %O" error
        | VisitedCalculationProblem error -> sprintf "Visited calculation problem: %O" error
        | FrontierInitProblem error -> sprintf "Frontier initialization problem: %O" error

let private op_add x y =
    match x, y with
    | Some u, Some v -> Some(min u v)
    | Some v, _
    | _, Some v -> Some v
    | _ -> None

let private op_mult x y =
    match x, y with
    | Some u, Some v -> Some(u + v)
    | _ -> None

/// Keeps a new distance only if it improves on the known one.
let private op_min x y =
    match x, y with
    | Some v, Some u -> if v < u then Some v else None
    | Some v, _ -> Some v
    | _ -> None

let ssspWith
    (backend: Backend<'e, 'c, 'vecC, 'matE, 'matU, 'vecF, 'matF, 'vErr, 'mErr, 'laErr>)
    (graph: 'matF)
    (startVertex: uint64<Vector.index>)
    : Result<'vecF, Error<'vErr, 'laErr>> =
    let rec inner frontier visited iterNum =
        if
            backend.fNvals frontier > 0UL<nvals>
            && iterNum <= int (backend.fLength frontier)
        then
            resultM {
                let! newFrontier =
                    backend.vxm op_add op_mult frontier graph
                    |> Result.mapError NewFrontierCalculationProblem

                let! frontier =
                    backend.fMap2 newFrontier visited op_min
                    |> Result.mapError FrontierCalculationProblem

                let! visited =
                    backend.fMap2 visited frontier op_add
                    |> Result.mapError VisitedCalculationProblem

                return! inner frontier visited (iterNum + 1)
            }
        else
            Ok visited

    resultM {
        let! frontier =
            backend.fFromCoordinates (uint64 (backend.fNcols graph) * 1UL<Vector.dataLength>) [ (startVertex, 0.0) ]
            |> Result.mapError FrontierInitProblem

        return! inner frontier frontier 0
    }
