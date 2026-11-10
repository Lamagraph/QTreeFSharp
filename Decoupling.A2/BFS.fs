/// BFS for approach A2 (docs/decoupling-proposals.md, section 5): the algorithm declares one
/// record holding exactly the operations it uses.
module A2.BFS

open Common
open Result

type Error<'vErr, 'laErr> =
    | NewFrontierCalculationProblem of 'laErr
    | FrontierCalculationProblem of 'vErr
    | VisitedCalculationProblem of 'vErr

    override this.ToString() =
        match this with
        | NewFrontierCalculationProblem error -> sprintf "New frontier calculation problem: %O" error
        | FrontierCalculationProblem error -> sprintf "Frontier calculation problem: %O" error
        | VisitedCalculationProblem error -> sprintf "Visited calculation problem: %O" error

/// Everything BFS needs from a representation: 'vec holds frontier values 'c, 'mat holds edge values 'b.
type Backend<'b, 'c, 'vec, 'mat, 'vErr, 'laErr> =
    { nvals: 'vec -> uint64<nvals>
      mapi: 'vec -> (uint64<Vector.index> -> 'c option -> 'c option) -> 'vec
      map2: 'vec -> 'vec -> ('c option -> 'c option -> 'c option) -> Result<'vec, 'vErr>
      mask: 'vec -> 'vec -> ('c option -> bool) -> Result<'vec, 'vErr>
      vxmiValues:
          ('c option -> 'c option -> 'c option)
              -> (uint64<Vector.index> * 'c -> uint64<Matrix.rowindex> * uint64<Matrix.colindex> * 'b -> 'c option)
              -> 'vec
              -> 'mat
              -> Result<'vec, 'laErr> }

let bfsWith
    (backend: Backend<'b, 'c, 'vec, 'mat, 'vErr, 'laErr>)
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: uint64<Vector.index> * 'c -> uint64<Matrix.rowindex> * uint64<Matrix.colindex> * 'b -> 'c option)
    (initVisited: uint64<Vector.index> -> 'c option -> 'c option)
    (graph: 'mat)
    (startVertices: 'vec)
    : Result<'vec, Error<'vErr, 'laErr>> =
    let rec inner frontier visited =
        if backend.nvals frontier > 0UL<nvals> then
            resultM {
                let! newFrontier =
                    backend.vxmiValues op_add op_mult frontier graph
                    |> Result.mapError NewFrontierCalculationProblem

                let! frontier =
                    backend.mask newFrontier visited Option.isNone
                    |> Result.mapError FrontierCalculationProblem

                let! visited =
                    backend.map2 visited frontier (fun oldVal newVal ->
                        match oldVal with
                        | Some _ -> oldVal
                        | None -> newVal)
                    |> Result.mapError VisitedCalculationProblem

                return! inner frontier visited
            }
        else
            Ok visited

    inner startVertices (backend.mapi startVertices initVisited)

let private firstSome x y =
    match x, y with
    | Some v, _
    | _, Some v -> Some v
    | _ -> None

let bfsLevelWith (backend: Backend<'b, uint64, 'vec, 'mat, 'vErr, 'laErr>) (graph: 'mat) (startVertices: 'vec) =
    let op_mult (_, level) _ = Some(level + 1UL)
    let toLevelZero _ v = v |> Option.map (fun _ -> 0UL)

    bfsWith backend firstSome op_mult toLevelZero graph (backend.mapi startVertices toLevelZero)

let bfsParentWith (backend: Backend<'b, uint64, 'vec, 'mat, 'vErr, 'laErr>) (graph: 'mat) (startVertices: 'vec) =
    let op_mult (parent: uint64<Vector.index>, _) _ = Some(uint64 parent)
    let initVisited (i: uint64<Vector.index>) v = v |> Option.map (fun _ -> uint64 i)

    bfsWith backend firstSome op_mult initVisited graph startVertices
