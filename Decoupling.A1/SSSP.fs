/// SSSP written against A1 capability records. Edge weights and distances are `float`.
module A1.SSSP

open Common
open Result
open A1.Backend

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
    (vec: VectorOps<float, 'vec, 'vErr>)
    (build: VectorBuildOps<float, 'vec, 'vErr>)
    (shape: MatrixShapeOps<'mat>)
    (kernel: VxmOps<float, float, float, 'vec, 'mat, 'vec, 'laErr>)
    (graph: 'mat)
    (startVertex: uint64<Vector.index>)
    : Result<'vec, Error<'vErr, 'laErr>> =
    let rec inner frontier visited iterNum =
        if vec.nvals frontier > 0UL<nvals> && iterNum <= int (vec.length frontier) then
            resultM {
                let! newFrontier =
                    kernel.vxm op_add op_mult frontier graph
                    |> Result.mapError NewFrontierCalculationProblem

                let! frontier =
                    vec.map2 newFrontier visited op_min
                    |> Result.mapError FrontierCalculationProblem

                let! visited = vec.map2 visited frontier op_add |> Result.mapError VisitedCalculationProblem
                return! inner frontier visited (iterNum + 1)
            }
        else
            Ok visited

    resultM {
        let! frontier =
            build.fromCoordinates (uint64 (shape.ncols graph) * 1UL<Vector.dataLength>) [ (startVertex, 0.0) ]
            |> Result.mapError FrontierInitProblem

        return! inner frontier frontier 0
    }
