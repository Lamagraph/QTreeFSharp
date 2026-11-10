/// BFS written against A1 capability records. Nothing here names a concrete representation.
module A1.BFS

open Common
open Result
open A1.Backend

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
    (vec: VectorOps<'c, 'vec, 'vErr>)
    (vmask: VectorMaskOps<'c, 'vec, 'vec, 'vErr>)
    (kernel: VxmiValuesOps<'b, 'c, 'vec, 'mat, 'laErr>)
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: uint64<Vector.index> * 'c -> uint64<Matrix.rowindex> * uint64<Matrix.colindex> * 'b -> 'c option)
    (initVisited: uint64<Vector.index> -> 'c option -> 'c option)
    (graph: 'mat)
    (startVertices: 'vec)
    : Result<'vec, Error<'vErr, 'laErr>> =
    let rec inner frontier visited =
        if vec.nvals frontier > 0UL<nvals> then
            resultM {
                let! newFrontier =
                    kernel.vxmiValues op_add op_mult frontier graph
                    |> Result.mapError NewFrontierCalculationProblem

                let! frontier =
                    vmask.mask newFrontier visited Option.isNone
                    |> Result.mapError FrontierCalculationProblem

                let! visited =
                    vec.map2 visited frontier (fun oldVal newVal ->
                        match oldVal with
                        | Some _ -> oldVal
                        | None -> newVal)
                    |> Result.mapError VisitedCalculationProblem

                return! inner frontier visited
            }
        else
            Ok visited

    inner startVertices (vec.mapi startVertices initVisited)

let private firstSome x y =
    match x, y with
    | Some v, _
    | _, Some v -> Some v
    | _ -> None

let bfsLevelWith
    (vec: VectorOps<uint64, 'vec, 'vErr>)
    (vmask: VectorMaskOps<uint64, 'vec, 'vec, 'vErr>)
    (kernel: VxmiValuesOps<'b, uint64, 'vec, 'mat, 'laErr>)
    (graph: 'mat)
    (startVertices: 'vec)
    =
    let op_mult (_, level) _ = Some(level + 1UL)
    let toLevelZero _ v = v |> Option.map (fun _ -> 0UL)

    bfsWith vec vmask kernel firstSome op_mult toLevelZero graph (vec.mapi startVertices toLevelZero)

let bfsParentWith
    (vec: VectorOps<uint64, 'vec, 'vErr>)
    (vmask: VectorMaskOps<uint64, 'vec, 'vec, 'vErr>)
    (kernel: VxmiValuesOps<'b, uint64, 'vec, 'mat, 'laErr>)
    (graph: 'mat)
    (startVertices: 'vec)
    =
    let op_mult (parent: uint64<Vector.index>, _) _ = Some(uint64 parent)
    let initVisited (i: uint64<Vector.index>) v = v |> Option.map (fun _ -> uint64 i)

    bfsWith vec vmask kernel firstSome op_mult initVisited graph startVertices
