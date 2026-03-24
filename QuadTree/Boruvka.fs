module Graph.Boruvka

open Common

type Error<'t1, 't2, 't3, 't4, 't5, 't6, 't7, 't8, 't9, 't10, 't11, 't12> =    
    | EdgesCalculationProblem of LinearAlgebra.Error<'t1, 't2, 't3>
    | CEdgesCalculationProblem of Vector.Error<'t4, 't5, 't6>
    | IndexCalculationProblem of Vector.Error<'t7, 't8, 't9>

let mst (graph:Matrix.SparseMatrix<_>) =
    eprintfn "MST CALLED nrows=%A ncols=%A nvals=%A" graph.nrows graph.ncols graph.nvals
    
    let op_add x y =
        match (x, y) with
        | Some(a, pa), Some(b, pb) -> 
            if a < b then Some(a, pa)
            elif b < a then Some(b, pb)
            elif pa <= pb then Some(a, pa)
            else Some(b, pb)
        | Some(a, pa), _ -> Some(a, pa)
        | _, Some(b, pb) -> Some(b, pb)
        | _ -> None

    let op_mult (i,x) (row,col,w) =
        Some(w,row)  // Store source vertex
    
    let length = uint64 graph.nrows * 1UL<Vector.dataLength>
    let parent = Vector.init length (fun i -> Some i)
    
    let rec inner (graph: Matrix.SparseMatrix<_>) (tree: Matrix.SparseMatrix<_>) parent iteration =
        eprintfn "Iter %d: graph=%A, tree=%A" iteration graph.nvals tree.nvals
        if graph.nvals > 0UL<nvals> then

            let edges = LinearAlgebra.vxmi_values op_add op_mult parent graph
            
            match edges with
            | Result.Failure(e) -> Result.Failure(EdgesCalculationProblem(e))
            | Result.Success(edges) ->
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

                    let t = Vector.gather cedges parent                    
                    let index = Vector.map2i t edges (fun i t e -> match (t,e) with |  Some v1, Some v2 when v1 = v2 -> Some i | _ -> None)
                    let index = Vector.scatter (Vector.empty length) index parent op_min
                    match index with 
                    | Result.Failure(e) -> Result.Failure(IndexCalculationProblem(e))
                    | Result.Success (index) ->
                        let index = Vector.gather index parent
                        
                        // Use compressed parent for filter
                        let parentCompressed = Vector.gather parent parent
                        
                        // edges[i] = (w, parent[source]) for minimum edge source -> i
                        // index[i] = i if i is a representative  
                        let filter i j = 
                            let i = uint64 i * 1UL<Vector.index>
                            let j = uint64 j * 1UL<Vector.index>
                            let edge = Vector.unsafeGet edges i
                            let idx = Vector.unsafeGet index i
                            let parentJ = Vector.unsafeGet parentCompressed j
                            match edge, idx, parentJ with 
                            | Some(w, parentSource), Some idxVal, Some pj -> 
                                uint64 idxVal = uint64 i && uint64 parentSource <> uint64 i && uint64 pj = uint64 parentSource
                            | _ -> false
                        
                        let tree = 
                            Matrix.map2i tree graph (
                                fun i j t g -> 
                                    match (t,g) with
                                    | Some t, _ -> Some t
                                    | None, Some g when filter i j -> Some g
                                    | _ -> None)

                        // Step 6: Update parent
                        let getPartnerIdx i =
                            let idxVal = Vector.unsafeGet index i
                            let edgeVal = Vector.unsafeGet edges i
                            match (idxVal, edgeVal) with
                            | Some idx', Some(weight, parentSource) when idx' = i -> 
                                Some(uint64 parentSource * 1UL<Vector.index>)
                            | _ -> None
                        
                        let scatterIndices: Vector.SparseVector<uint64<Vector.index>> = Vector.init length getPartnerIdx
                        let scatterValues: Vector.SparseVector<uint64<Vector.index>> = Vector.init length (fun i -> Some i)
                        
                        let parentResult = Vector.scatter parent scatterIndices scatterValues (fun old newVal -> newVal)
                        
                        match parentResult with
                        | Result.Failure(e) -> Result.Failure(IndexCalculationProblem(e))
                        | Result.Success(parent) ->
                            // Path compression: gather multiple times
                            let parent = Vector.gather parent parent
                            let parent = Vector.gather parent parent
                            let parent = Vector.gather parent parent
                            let parent = Vector.gather parent parent
                            let parent = Vector.gather parent parent
                            let parent = Vector.gather parent parent
                            let parent = Vector.gather parent parent
                            let parent = Vector.gather parent parent
                            let parent = Vector.gather parent parent
                            let parent = Vector.gather parent parent
                            
                            // Step 7: Filter graph to remove intra-component edges
                            // First do MORE compressions to get actual roots
                            let rec compress p iter =
                                if iter > 20 then p else
                                let p2 = Vector.gather p p
                                compress p2 (iter + 1)
                            let parentForFilter = compress parent 0
                            
                            // Debug: print parent for filtering
                            if iteration < 2 then
                                eprintfn "Step7 parent for filtering:"
                                for i in [0UL;1UL;2UL;3UL;4UL;5UL;6UL] do
                                    let p = Vector.unsafeGet parentForFilter (i * 1UL<Vector.index>)
                                    eprintfn "  parent[%d]=%A" i p
                            
                            let graphFilter i j = 
                                let i = uint64 i * 1UL<Vector.index>
                                let j = uint64 j * 1UL<Vector.index>
                                let parent_i = Vector.unsafeGet parentForFilter i
                                let parent_j = Vector.unsafeGet parentForFilter j
                                match (parent_i, parent_j) with
                                | Some v1, Some v2 when v1 <> v2 -> true
                                | _ -> false

                            let graph = Matrix.mapi graph (fun i j v -> if graphFilter i j then v else None)

                            inner graph tree parent (iteration + 1)

                
        else
            Result.Success tree

    inner graph (Matrix.empty graph.nrows graph.ncols) parent 0
