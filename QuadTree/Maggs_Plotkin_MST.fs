module Graph.Maggs_Plotkin_MST

open Common
open Result
open Matrix


type Error =
    | DiagAdditionProblem of Matrix.Error
    | MSTComputationProblem of Matrix.Error
    | ClosureComputationProblem of LinearAlgebra.Error

let mst (graph: Matrix.SparseMatrix<'a>) =

    let diag =
        let zero = Unchecked.defaultof<'a>

        Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                graph.nrows,
                graph.ncols,
                [ for i in 0UL .. uint64 graph.nrows - 1UL -> (i * 1UL<rowindex>, i * 1UL<colindex>, zero) ]
            )
        )

    let _max x y =
        match (x, y) with
        | Some x, Some y -> max x y |> Some
        | _ -> None

    let _min x y =
        match (x, y) with
        | Some x, Some y -> min x y |> Some
        | Some x, None
        | None, Some x -> Some x
        | _ -> None

    resultM {
        let! graph =
            Matrix.map2 graph diag (fun x y ->
                match y with
                | None -> x
                | _ -> y)
            |> Result.mapError DiagAdditionProblem

        let graph =
            Matrix.mapi graph (fun i j v ->
                Option.map (fun x -> x, min (uint64 i) (uint64 j), max (uint64 i) (uint64 j)) v)

        let! closure =
            let rec compute (matrix: SparseMatrix<_>) =
                let nnz = matrix.nvals

                resultM {

                    let! step = LinearAlgebra.mxm _min _max matrix matrix

                    if nnz = step.nvals && matrix.storage = step.storage then
                        return step
                    else
                        return! compute step
                }

            compute graph |> Result.mapError ClosureComputationProblem

        let! mst =
            Matrix.map2i graph closure (fun i j x y ->
                if uint64 i = uint64 j then
                    None
                elif x = y then
                    match x with
                    | Some(w, _, _) -> Some(w)
                    | _ -> None
                else
                    None)
            |> Result.mapError MSTComputationProblem

        return mst
    }
