/// Generic breadth-first search, independent of the vector/matrix representation.
/// Representation-specific operations are supplied via `BfsOps`; concrete
/// specializations live under `Graph.Impl.*` (e.g. `Graph.Impl.QuadTree.BFS`).
module Graph.BFS

open Common
open Result

type Error<'vErr, 'laErr> =
    | NewFrontierCalculationProblem of 'laErr
    | FrontierCalculationProblem of 'vErr
    | VisitedCalculationProblem of 'vErr

/// Indexed semiring multiplication used by `vxmiValues`:
/// (frontier index, frontier value) × (row, column, edge value) → new frontier value.
type IndexedMult<'vertex, 'edge> =
    uint64<Vector.index> * 'vertex -> uint64<Matrix.rowindex> * uint64<Matrix.colindex> * 'edge -> 'vertex option

/// Everything BFS requires from a representation.
/// `'edge` is the edge value type, `'vertex` the frontier/visited value type,
/// `'vec`/`'mat` the concrete vector/matrix types, `'vErr`/`'laErr` their errors.
type BfsOps<'edge, 'vertex, 'vec, 'mat, 'vErr, 'laErr> =
    { nvals: 'vec -> uint64<nvals>
      mapi: 'vec -> (uint64<Vector.index> -> 'vertex option -> 'vertex option) -> 'vec
      map2: 'vec -> 'vec -> ('vertex option -> 'vertex option -> 'vertex option) -> Result<'vec, 'vErr>
      mask: 'vec -> 'vec -> ('vertex option -> bool) -> Result<'vec, 'vErr>
      vxmiValues:
          ('vertex option -> 'vertex option -> 'vertex option)
              -> IndexedMult<'vertex, 'edge>
              -> 'vec
              -> 'mat
              -> Result<'vec, 'laErr> }

let bfs
    (ops: BfsOps<'edge, 'vertex, 'vec, 'mat, 'vErr, 'laErr>)
    (op_add: 'vertex option -> 'vertex option -> 'vertex option)
    (op_mult: IndexedMult<'vertex, 'edge>)
    (initVisited: uint64<Vector.index> -> 'vertex option -> 'vertex option)
    (graph: 'mat)
    (startVertices: 'vec)
    : Result<'vec, Error<'vErr, 'laErr>> =
    let initialVisited = ops.mapi startVertices initVisited

    let rec inner (frontier: 'vec) (visited: 'vec) =
        if ops.nvals frontier > 0UL<nvals> then
            resultM {
                let! newFrontier =
                    ops.vxmiValues op_add op_mult frontier graph
                    |> Result.mapError NewFrontierCalculationProblem

                let! frontier =
                    ops.mask newFrontier visited (fun x -> x.IsNone)
                    |> Result.mapError FrontierCalculationProblem

                let! visited =
                    ops.map2 visited frontier (fun oldVal newVal ->
                        match oldVal with
                        | Some _ -> oldVal
                        | None -> newVal)
                    |> Result.mapError VisitedCalculationProblem

                return! inner frontier visited
            }
        else
            Ok visited

    inner startVertices initialVisited

let bfsLevel (ops: BfsOps<'edge, uint64, 'vec, 'mat, 'vErr, 'laErr>) (graph: 'mat) (startVertices: 'vec) =
    let op_add x y =
        match (x, y) with
        | Some(v), _
        | _, Some(v) -> Some(v)
        | _ -> None

    let op_mult (_, vp) (_, _, _) = Some(vp + 1UL)

    let initVisited _ v = v |> Option.map (fun _ -> 0UL)

    let frontier0 = ops.mapi startVertices (fun _ v -> v |> Option.map (fun _ -> 0UL))

    bfs ops op_add op_mult initVisited graph frontier0

let bfsParent (ops: BfsOps<'edge, uint64, 'vec, 'mat, 'vErr, 'laErr>) (graph: 'mat) (startVertices: 'vec) =
    let op_add x y =
        match (x, y) with
        | Some(v), _
        | _, Some(v) -> Some(v)
        | _ -> None

    let op_mult (vi, _) (_, _, _) = Some(uint64 vi)

    let initVisited i v = v |> Option.map (fun _ -> uint64 i)

    bfs ops op_add op_mult initVisited graph startVertices
