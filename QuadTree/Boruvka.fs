module Graph.Boruvka

open Common


let printMatrixCoordinate (matrix: Matrix.SparseMatrix<_>) =
    printfn "Matrix:"
    printfn "   Nvals: %A" matrix.nvals
    printfn "   Data: %A" (Matrix.toCoordinateList matrix).list

let printVector (vector: Vector.SparseVector<_>) =
    printfn "Vector:"
    printfn "   Nvals: %A" vector.nvals
    printfn "   Data: %A" (Vector.toCoordinateList vector).data

let mst (graph:Matrix.SparseMatrix<_>) =

    let op_mult (i,x) (row,col,w) =
        Some(w,row)

    let op_min x y = 
        match (x, y) with
        | Some v, Some u -> Some (min v u) 
        | Some v, _ -> Some v
        | None, Some v -> Some v
        | _ -> None
      
    let fixPoint p =
        let rec inner p iter = 
            let p2 = Vector.gather p p
            if p2 = p then p, iter else inner p2 (iter+1)
        let res, iters = inner p 0
        printfn "Iterations to update parents: %A" iters
        res

    let treeFilter edges index = 
        fun i j g ->
            let i = uint64 i * 1UL<Vector.index>
            let j = uint64 j * 1UL<Vector.index>
            let edge = Vector.unsafeGet edges i
            let idx = Vector.unsafeGet index i
            let result = 
                match edge, idx with 
                | Some(w, dst), Some idxVal-> 
                    g = w && idxVal = i && uint64 dst = uint64 j
                | _ -> false
            if result then printfn "TREE FILTER: edge (%A,%A) -> tree" i j
            result
    
    let graphFilter parent = 
        fun i j -> 
            let i = uint64 i * 1UL<Vector.index>
            let j = uint64 j * 1UL<Vector.index>
            let parent_i = Vector.unsafeGet parent i
            let parent_j = Vector.unsafeGet parent j
            let result = 
                match (parent_i, parent_j) with
                | Some v1, Some v2 when v1 <> v2 -> true
                | _ -> false
            
            //if result then printfn "GRAPH FILTER: keep edge (%A,%A)" i j
            result

    let length = uint64 graph.nrows * 1UL<Vector.dataLength>
    
    // Initially each vertex is its own component
    // Each component can be identified by its root super-vertex, parent)
    let parent = Vector.init length (fun i -> Some i)
    
    let rec inner (graph: Matrix.SparseMatrix<_>) (tree: Matrix.SparseMatrix<_>) parent iteration =
        printfn "=== Iter %d: graph=%A, tree=%A ===" iteration graph.nvals tree.nvals
        printfn "=== Graph ==="
        printMatrixCoordinate graph
        printfn "Parent at start of iter %d:" iteration
        printVector parent
        if graph.nvals > 0UL<nvals> then

            
            // Cheapest outgoing edge for each vertex 
            // For each vertex j, find the smallest weight edge (i, j, w)
            // such that i and j are in different components.
            // Because graph contains only cross‑component edges,
            // we simply take the min over all neighbors.
            let edges = LinearAlgebra.vxmi_values op_min op_mult parent graph
           
            printfn "=== Edges ==="
            printVector edges 

            // Per‑component cheapest edge
            // For each component, keep the smallest edges among its vertices.
            let cedges = Vector.scatter (Vector.empty length) edges parent op_min
            
            printfn "=== Component Edges ==="
            printVector cedges

            // Propagate component's cheapest edge to all its vertices
            // Each vertex gets its component's edge
            let t = Vector.gather cedges parent
            
            // Identify a representative vertex for each component
            // For each vertex, if its own edge is the component's cheapest, mark it.
            let index = Vector.map2i t edges (fun i t e -> match (t,e) with |  Some v1, Some v2 when v1 = v2 -> Some i | _ -> None)
            // Among the marked vertices in a component, keep the smallest index.
            let index = Vector.scatter (Vector.empty length) index parent op_min
            // now each vertex knows its component's representative
            let index = Vector.gather index parent
            
            printfn "=== Index ==="
            printVector index

            // Add selected edges to the MST tree
            // An edge (i, j, w) is added if vertex i is the representative for its component
            // and (i, j, w) is the cheapest edge of that component.
            let treeFilter = treeFilter edges index
            let tree = 
                Matrix.map2i tree graph (
                    fun i j t g -> 
                        match (t,g) with
                        | Some t, _ -> Some t
                        | None, Some g when treeFilter i j g -> Some g
                        | _ -> None)

            // Compute new parent assignments (merge components)
            // For each component representative i with cheapest edge (w, j), we want to merge
            // the component of i with the component of j. Choose the smaller root.
            let data_for_update_parent = 
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

            printfn "=== Data for update parent ==="
            printVector data_for_update_parent
            // Apply the updates
            let initial_parent_update = 
                Vector.foldValues  data_for_update_parent (fun state (i,v) -> 
                    
                            Vector.update state i (Some v) (fun old _new -> _new)
                    )
                    parent

            printfn "=== Initial parent update ==="
            printVector initial_parent_update
            
            let parent = Vector.scatter parent initial_parent_update parent op_min
                
            printfn "=== Initially updated parent ==="
            printVector parent
            
            // Then ensure that all vertices in a merged component point to the same root.
            // This is done by a fixpoint (path compression) that repeatedly gathers parents.
            let parent = fixPoint parent
            
            printfn "=== Parent before data propagation ==="
            printVector parent
            
            // Filter the graph to keep only edges between different components
            let graphFilter = graphFilter parent
            let graph = Matrix.mapi graph (fun i j v -> if graphFilter i j then v else None)

            inner graph tree parent (iteration + 1)
                
        else
            tree

    inner graph (Matrix.empty graph.nrows graph.ncols) parent 0
