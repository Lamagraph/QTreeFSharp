module Graph.Boruvka

open Common


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



type Error<'t1, 't2, 't3, 't4, 't5, 't6, 't7, 't8, 't9, 't10, 't11, 't12> =    
    | EdgesCalculationProblem of LinearAlgebra.Error<'t1, 't2, 't3>
    | CEdgesCalculationProblem of Vector.Error<'t4, 't5, 't6>
    | IndexCalculationProblem of Vector.Error<'t7, 't8, 't9>
    | ScatterProblem of Vector.Error<'t10, 't11, 't12>

let mst (graph:Matrix.SparseMatrix<_>) =
    printfn "MST CALLED nrows=%A ncols=%A nvals=%A" graph.nrows graph.ncols graph.nvals
    
    let op_add x y =
        match (x, y) with
        | Some(a, pa), Some(b, pb) -> 
            Some (min (a,pa) (b,pb))
           (* if a < b then Some(a, pa) 
            elif b < a then Some(b, pb)
            elif pa <= pb then Some(a, pa)
            else Some(b, pb)*)
        | Some(a, pa), _ -> Some(a, pa)
        | _, Some(b, pb) -> Some(b, pb)
        | _ -> None

    let op_mult (i,x) (row,col,w) =
        Some(w,row)  // Store source vertex
    
    let length = uint64 graph.nrows * 1UL<Vector.dataLength>
    let parent = Vector.init length (fun i -> Some i)
    
    let rec inner (graph: Matrix.SparseMatrix<_>) (tree: Matrix.SparseMatrix<_>) parent iteration =
        printfn "=== Iter %d: graph=%A, tree=%A ===" iteration graph.nvals tree.nvals
        printfn "=== Graph ==="
        printMatrixCoordinate graph
        if iteration < 2 then
            printfn "Parent at start of iter %d:" iteration
            for i in 0UL..6UL do
                let p = Vector.unsafeGet parent (i * 1UL<Vector.index>)
                printfn "  parent[%d]=%A" i p
        if graph.nvals > 0UL<nvals> then

            let edges = LinearAlgebra.vxmi_values op_add op_mult parent graph            
            match edges with
            | Result.Failure(e) -> Result.Failure(EdgesCalculationProblem(e))
            | Result.Success(edges) ->
                printfn "=== Edges ==="
                printVector edges 

                let op_min x y =
                    match (x, y) with
                    | Some v, Some u -> if v < u then Some v else None
                    | Some v, _ -> Some v
                    | None, Some v -> Some v
                    | _ -> None

                let cedges = 
                    Vector.scatter (Vector.empty length) edges parent op_add

                match cedges with
                | Result.Failure(e) -> Result.Failure(CEdgesCalculationProblem(e))
                | Result.Success(cedges) ->
                    printfn "=== Component Edges ==="
                    printVector cedges

                    let t = Vector.gather cedges parent                    
                    let index = Vector.map2i t edges (fun i t e -> match (t,e) with |  Some v1, Some v2 when v1 = v2 -> Some i | _ -> None)
                    let index = Vector.scatter (Vector.empty length) index parent op_min
                    match index with 
                    | Result.Failure(e) -> Result.Failure(IndexCalculationProblem(e))
                    | Result.Success (index) ->
                        //printfn "=== Index ==="
                        //printVector index
                        let index = Vector.gather index parent
                        printfn "=== Index 2 ==="
                        printVector index
                        // Use compressed parent for filter
                        //let parentCompressed = Vector.gather parent parent
                        //let parentCompressed = Vector.gather parentCompressed parentCompressed
                        //let parentCompressed = Vector.gather parentCompressed parentCompressed
                        //let parentCompressed = Vector.gather parentCompressed parentCompressed
                        //let parentCompressed = Vector.gather parentCompressed parentCompressed
                        //let parentCompressed = Vector.gather parentCompressed parentCompressed
                        
                        printfn "=== parent ==="
                        printVector parent


                        // edges[i] = (w, parent[source]) for minimum edge source -> i
                        // index[i] = i if i is a representative  
                        // Filter: add edge (i,j) only if i is rep AND j = src AND j is in DIFFERENT component than i
                        let filter i j g = 
                            let i = uint64 i * 1UL<Vector.index>
                            let j = uint64 j * 1UL<Vector.index>
                            printfn "Edge for filter: %A %A %A" i j g
                            let edge = Vector.unsafeGet edges i
                            let idx = Vector.unsafeGet index i
                            let parent_j = Vector.unsafeGet parent j
                            let result = 
                                match edge, idx, parent_j with 
                                | Some(w, dst), Some idxVal, Some pi -> 
                                    // i is rep (parent[i]=i), source != i, j = src
                                    //(uint64 pi) = (uint64 i) && (uint64 src) <> (uint64 i) && (uint64 j) = (uint64 src)
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

                        // Step 6: Update parent
                        (*
                        let getPartnerIdx i =
                            let idxVal = Vector.unsafeGet index i
                            let edgeVal = Vector.unsafeGet edges i
                            if iteration < 2 then
                                printfn "  getPartnerIdx %d: idxVal=%A, edgeVal=%A" i idxVal edgeVal
                            match (idxVal, edgeVal) with
                            | Some idx', Some(weight, parentSource) when idx' = i -> 
                                Some(uint64 parentSource * 1UL<Vector.index>)
                            | _ -> None
                        
                        let scatterIndices: Vector.SparseVector<uint64<Vector.index>> = Vector.init length getPartnerIdx
                        let scatterValues: Vector.SparseVector<uint64<Vector.index>> = Vector.init length (fun i -> Some i)
                        
                        let parentResult = Vector.scatter parent scatterIndices scatterValues (fun old newVal -> 
                            match old, newVal with
                            | Some o, Some n -> if o < n then Some o else Some n
                            | Some o, None -> Some o
                            | None, Some n -> Some n
                            | _ -> None)
                        *)
                        let _parent = 
                            Vector.map2i edges index 
                                (fun i e idx ->
                                    match e,idx with 
                                    | Some (v,j), Some (_i) when _i = i ->
                                        let j = uint64 j * 1UL<Vector.index>
                                        let parent = Vector.unsafeGet parent (min j i)
                                        match parent with 
                                        | Some p -> Some (max j i, p)
                                        | x -> failwithf "Unreachable: %A" x
                                    | _ -> None
                                )

                        printfn "=== _parent ==="
                        printVector _parent

                        let parentResult = 
                            Vector.foldValues  _parent (fun state (i,v) -> 
                                match state with 
                                | Result.Success state ->
                                     Vector.update state i (Some v) (fun old _new -> _new)
                                | Result.Failure x -> Result.Failure x)
                                (Result.Success parent)

                        match parentResult with
                        | Result.Failure(e) -> Result.Failure(IndexCalculationProblem(e))
                        | Result.Success(__parent) ->
                            printfn "=== parentResult ==="
                            printVector __parent
                            // Path compression: fix-point iteration using vector length
                            let vecLength = uint64 parent.length
                            let rec fixPoint p iter =
                                let p2 = Vector.gather p p
                                if p2 = p then p else fixPoint p2 1
                            let op_min x y =
                                match (x, y) with
                                | Some v, Some u -> if v < u then Some v else Some u
                                | Some v, _ -> Some v
                                | None, Some v -> Some v
                                | _ -> None
                            let parent = Vector.scatter parent __parent parent op_min                            
                            match parent with 
                            | Result.Failure x -> ScatterProblem x |> Result.Failure
                            | Result.Success parent -> 
                                printfn "=== parent' ==="
                                printVector parent
                                let parent = fixPoint parent 0
                                
                                printfn "=== Parent for filter ==="
                                printVector parent
                                let graphFilter i j = 
                                    let i = uint64 i * 1UL<Vector.index>
                                    let j = uint64 j * 1UL<Vector.index>
                                    let parent_i = Vector.unsafeGet parent i
                                    let parent_j = Vector.unsafeGet parent j
                                    let result = 
                                        match (parent_i, parent_j) with
                                        | Some v1, Some v2 when v1 <> v2 -> true
                                        | _ -> false
                                    if iteration < 2 && result then
                                        printfn "GRAPH FILTER iter %d: keep edge (%d,%d)" iteration (i/1UL<Vector.index>) (j/1UL<Vector.index>)
                                    result

                                let graph = Matrix.mapi graph (fun i j v -> if graphFilter i j then v else None)

                                inner graph tree parent (iteration + 1)

                
        else
            Result.Success tree

    inner graph (Matrix.empty graph.nrows graph.ncols) parent 0
