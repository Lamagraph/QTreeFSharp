/// SSSP for approach A2. Its record redeclares `nvals` and `map2` that BFS already declared:
/// that duplication is the price of A2.
module A2.SSSP

open Common
open Result

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

/// Everything SSSP needs: 'vec holds `float` distances, 'mat holds `float` weights.
type Backend<'vec, 'mat, 'vErr, 'laErr> =
    { length: 'vec -> uint64<Vector.dataLength>
      nvals: 'vec -> uint64<nvals>
      map2: 'vec -> 'vec -> (float option -> float option -> float option) -> Result<'vec, 'vErr>
      fromCoordinates: uint64<Vector.dataLength> -> (uint64<Vector.index> * float) list -> Result<'vec, 'vErr>
      ncols: 'mat -> uint64<Matrix.ncols>
      vxm:
          (float option -> float option -> float option)
              -> (float option -> float option -> float option)
              -> 'vec
              -> 'mat
              -> Result<'vec, 'laErr> }

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
    (backend: Backend<'vec, 'mat, 'vErr, 'laErr>)
    (graph: 'mat)
    (startVertex: uint64<Vector.index>)
    : Result<'vec, Error<'vErr, 'laErr>> =
    let rec inner frontier visited iterNum =
        if backend.nvals frontier > 0UL<nvals> && iterNum <= int (backend.length frontier) then
            resultM {
                let! newFrontier =
                    backend.vxm op_add op_mult frontier graph
                    |> Result.mapError NewFrontierCalculationProblem

                let! frontier =
                    backend.map2 newFrontier visited op_min
                    |> Result.mapError FrontierCalculationProblem

                let! visited =
                    backend.map2 visited frontier op_add
                    |> Result.mapError VisitedCalculationProblem

                return! inner frontier visited (iterNum + 1)
            }
        else
            Ok visited

    resultM {
        let! frontier =
            backend.fromCoordinates (uint64 (backend.ncols graph) * 1UL<Vector.dataLength>) [ (startVertex, 0.0) ]
            |> Result.mapError FrontierInitProblem

        return! inner frontier frontier 0
    }
