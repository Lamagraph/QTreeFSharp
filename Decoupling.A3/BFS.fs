/// BFS for approach A3. It takes the full backend, although it uses only the BFS fields.
module A3.BFS

open Common
open Result
open A3.Backend

type Error<'vErr, 'laErr> =
    | NewFrontierCalculationProblem of 'laErr
    | FrontierCalculationProblem of 'vErr
    | VisitedCalculationProblem of 'vErr

    override this.ToString() =
        match this with
        | NewFrontierCalculationProblem error -> sprintf "New frontier calculation problem: %O" error
        | FrontierCalculationProblem error -> sprintf "Frontier calculation problem: %O" error
        | VisitedCalculationProblem error -> sprintf "Visited calculation problem: %O" error

let bfsWith
    (backend: Backend<'e, 'c, 'vecC, 'matE, 'matU, 'vecF, 'matF, 'vErr, 'mErr, 'laErr>)
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: uint64<Vector.index> * 'c -> uint64<Matrix.rowindex> * uint64<Matrix.colindex> * 'e -> 'c option)
    (initVisited: uint64<Vector.index> -> 'c option -> 'c option)
    (graph: 'matE)
    (startVertices: 'vecC)
    : Result<'vecC, Error<'vErr, 'laErr>> =
    let rec inner frontier visited =
        if backend.vNvals frontier > 0UL<nvals> then
            resultM {
                let! newFrontier =
                    backend.vxmiValues op_add op_mult frontier graph
                    |> Result.mapError NewFrontierCalculationProblem

                let! frontier =
                    backend.vMask newFrontier visited Option.isNone
                    |> Result.mapError FrontierCalculationProblem

                let! visited =
                    backend.vMap2 visited frontier (fun oldVal newVal ->
                        match oldVal with
                        | Some _ -> oldVal
                        | None -> newVal)
                    |> Result.mapError VisitedCalculationProblem

                return! inner frontier visited
            }
        else
            Ok visited

    inner startVertices (backend.vMapi startVertices initVisited)

let private firstSome x y =
    match x, y with
    | Some v, _
    | _, Some v -> Some v
    | _ -> None

let bfsLevelWith
    (backend: Backend<'e, uint64, 'vecC, 'matE, 'matU, 'vecF, 'matF, 'vErr, 'mErr, 'laErr>)
    (graph: 'matE)
    (startVertices: 'vecC)
    =
    let op_mult (_, level) _ = Some(level + 1UL)
    let toLevelZero _ v = v |> Option.map (fun _ -> 0UL)

    bfsWith backend firstSome op_mult toLevelZero graph (backend.vMapi startVertices toLevelZero)

let bfsParentWith
    (backend: Backend<'e, uint64, 'vecC, 'matE, 'matU, 'vecF, 'matF, 'vErr, 'mErr, 'laErr>)
    (graph: 'matE)
    (startVertices: 'vecC)
    =
    let op_mult (parent: uint64<Vector.index>, _) _ = Some(uint64 parent)
    let initVisited (i: uint64<Vector.index>) v = v |> Option.map (fun _ -> uint64 i)

    bfsWith backend firstSome op_mult initVisited graph startVertices
