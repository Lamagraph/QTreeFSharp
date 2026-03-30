module Graph.Boruvka

open Common
open Result


type Error =
    | EdgesCalculationProblem of LinearAlgebra.Error
    | CEdgesCalculationProblem of Vector.Error
    | IndexCalculationProblem of Vector.Error
    | ScatterProblem of Vector.Error
    | FoldValuesError of Vector.Error

let mapError (err: LinearAlgebra.Error) = EdgesCalculationProblem err
let mapError' (err: Vector.Error) = CEdgesCalculationProblem err
let mapError'' (err: Vector.Error) = IndexCalculationProblem err
let mapError''' (err: Vector.Error) = ScatterProblem err
let mapError'''' (err: Vector.Error) = FoldValuesError err

let printMatrixCoordinate (matrix: Matrix.SparseMatrix<_>) =
    printfn "Matrix:"
    printfn "   Rows: %A" matrix.nrows
    printfn "   Columns: %A" matrix.ncols
    printfn "   Nvals: %A" matrix.nvals
    printfn "   Storage:"
    printfn "      size: %A" matrix.storage.size
    printfn "      Data: %A" (Matrix.toCoordinateList matrix).list

let printVector (vector: Vector.SparseVector<_>) =
    printfn "Vector:"
    printfn "   Length: %A" vector.length
    printfn "   Nvals: %A" vector.nvals
    printfn "   Storage:"
    printfn "      Size: %A" vector.storage.size
    printfn "      Data: %A" (Vector.toCoordinateList vector).data

let mst (graph:Matrix.SparseMatrix<_>) =
    printfn "MST CALLED nrows=%A ncols=%A nvals=%A" graph.nrows graph.ncols graph.nvals
    
    let op_add x y =
        match (x, y) with
        | Some(a, pa), Some(b, pb) -> 
            Some (min (a,pa) (b,pb))
        | Some(a, pa), _ -> Some(a, pa)
        | _, Some(b, pb) -> Some(b, pb)
        | _ -> None

    let op_mult (i,x) (row,col,w) =
        Some(w,row)
    
    let length = uint64 graph.nrows * 1UL<Vector.dataLength>
    printfn "Length = %A" length
    let parentInit = Vector.init length (fun i -> Some i)
    
    let rec inner (graph: Matrix.SparseMatrix<_>) (tree: Matrix.SparseMatrix<_>) parent iteration =
        printfn "=== Iter %d: graph=%A, tree=%A ===" iteration graph.nvals tree.nvals
        printfn "=== Graph ==="
        printMatrixCoordinate graph
        printfn "Parent at start of iter %d:" iteration
        printVector parent
        if graph.nvals > 0UL<nvals> then

            let edgesResult = LinearAlgebra.vxmi_values op_add op_mult parent graph
            match edgesResult with
            | Error e -> Error(EdgesCalculationProblem e)
            | Ok edges ->
                printfn "=== Edges ==="
                printVector edges 

                let op_min x y =
                    match (x, y) with
                    | Some v, Some u -> if v < u then Some v else None
                    | Some v, _ -> Some v
                    | None, Some v -> Some v
                    | _ -> None

                let cedgesResult = Vector.scatter (Vector.empty length) edges parent op_add
                match cedgesResult with
                | Error e -> Error(CEdgesCalculationProblem e)
                | Ok cedges ->
                    printfn "=== Component Edges ==="
                    printVector cedges

                    let t = Vector.gather cedges parent
                    let indexInner = Vector.map2i t edges (fun i t e -> match (t,e) with |  Some v1, Some v2 when v1 = v2 -> Some i | _ -> None)
                    let indexResult = Vector.scatter (Vector.empty length) indexInner parent op_min
                    match indexResult with
                    | Error e -> Error(IndexCalculationProblem e)
                    | Ok index ->
                        let index = Vector.gather index parent
                        printfn "=== Index 2 ==="
                        printVector index
                                
                        printfn "=== parent ==="
                        printVector parent

                        let filter i j g = 
                            let i = uint64 i * 1UL<Vector.index>
                            let j = uint64 j * 1UL<Vector.index>
                            let edge = Vector.unsafeGet edges i
                            let idx = Vector.unsafeGet index i
                            let parent_j = Vector.unsafeGet parent j
                            let result = 
                                match edge, idx, parent_j with 
                                | Some(w, dst), Some idxVal, Some pi -> 
                                    g = w && idxVal = i && uint64 dst = uint64 j
                                | _ -> false
                            if result then
                                printfn "TREE FILTER iter %d: edge (%d,%d) -> tree" iteration (i/1UL<Vector.index>) (j/1UL<Vector.index>)
                            result
                                
                        let tree = 
                            Matrix.map2i tree graph (
                                fun i j t g -> 
                                    match (t,g) with
                                    | Some t, _ -> Some t
                                    | None, Some g when filter i j g -> Some g
                                    | _ -> None)

                        let _parentInner = 
                            Vector.map2i edges index 
                                (fun i e idx ->
                                    match e,idx with 
                                    | Some (v,j), Some (_i) when _i = i ->
                                        let j = uint64 j * 1UL<Vector.index>
                                        let parent_i = Vector.unsafeGet parent i
                                        let parent_j = Vector.unsafeGet parent j
                                        match parent_i,parent_j with 
                                        | Some p_i, Some p_j -> 
                                            if p_i < p_j then Some (j, p_i) else Some (i, p_j)
                                        | x -> failwithf "Unreachable: %A" x
                                    | _ -> None
                                )

                        printfn "=== _parent ==="
                        printVector _parentInner

                        let parentUpdateResult = 
                            Vector.foldValues _parentInner (fun state (i,v) -> 
                                match state with 
                                | Ok state ->
                                    let updateResult = Vector.update state i (Some v) (fun old _new -> _new)
                                    match updateResult with
                                    | Ok u -> Ok u
                                    | Error e -> Error e
                                | Error e -> Error e)
                                (Ok parent)

                        match parentUpdateResult with
                        | Error e -> Error(FoldValuesError e)
                        | Ok __parent ->
                            printfn "=== parentResult ==="
                            printVector __parent
                            
                            let rec fixPoint p =
                                let p2 = Vector.gather p p
                                if p2 = p then p else fixPoint p2
                            let op_min2 x y =
                                match (x, y) with
                                | Some v, Some u -> if v < u then Some v else Some u
                                | Some v, _ -> Some v
                                | None, Some v -> Some v
                                | _ -> None
                            let parentScatterResult = Vector.scatter parent __parent parent op_min2
                            match parentScatterResult with
                            | Error e -> Error(ScatterProblem e)
                            | Ok parentNew -> 
                                printfn "=== parent' ==="
                                printVector parentNew
                                let parentFixed = fixPoint parentNew
                                
                                printfn "=== Parent for filter ==="
                                printVector parentFixed
                                let graphFilter i j = 
                                    let i = uint64 i * 1UL<Vector.index>
                                    let j = uint64 j * 1UL<Vector.index>
                                    let parent_i = Vector.unsafeGet parentFixed i
                                    let parent_j = Vector.unsafeGet parentFixed j
                                    let result = 
                                        match (parent_i, parent_j) with
                                        | Some v1, Some v2 when v1 <> v2 -> true
                                        | _ -> false
                                    if iteration < 2 && result then
                                        printfn "GRAPH FILTER iter %d: keep edge (%d,%d)" iteration (i/1UL<Vector.index>) (j/1UL<Vector.index>)
                                    result

                                let graph = Matrix.mapi graph (fun i j v -> if graphFilter i j then v else None)

                                inner graph tree parentFixed (iteration + 1)

        else
            Ok tree

    inner graph (Matrix.empty graph.nrows graph.ncols) parentInit 0
