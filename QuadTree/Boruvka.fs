module Graph.Boruvka

open Common

(*
Вход: граф G = (V, E, w), матрица смежности S, n = |V|
Выход: множество рёбер МОД T

iota <- [0, 1, 2, ..., n-1]    // вектор индексов

parent[u] <- u    для всех u in 0..n-1
T <- empty

while S not empty do {

    // Шаг 1. Минимальное ребро каждой вершины
    //        mxv над полукольцом combMin:
    //        edge[u] = min{ (w, parent[v]) | (u,v,w) in S }

    edge <- mxv(S, parent)

    // Шаг 2. Минимальное ребро каждой компоненты
    //        scatter-reduce: cedge[parent[u]] = min по всем u в компоненте

    cedge <- scatter(edge, parent, min)

    // Шаг 3. Распространить минимум компоненты на все её вершины
    //        gather: t[u] = cedge[parent[u]]

    t <- gather(cedge, parent)

    // Шаг 4. Выбрать одного представителя на компоненту

    mask  <- eWiseMult(edge, t, ==)
    index <- assign([n, n, ..., n], iota, mask)
    //       index[u] <- u  если mask[u], иначе n

    index <- scatter(index, parent, min)    // минимальный представитель в компоненте
    index <- gather(index, parent)          // broadcast на все вершины компоненты

    // Шаг 5. Добавить выбранные рёбра в МОД

    (weight, partner) <- extract_tuples(edge)

    s1 = fun i j ->
            weight[i] == S(i,j)
            && parent[j] == partner[i]
            && index[i] == i

    T <- T ∪ select(S, s1)

    // Шаг 6. Обновить компоненты (до фильтрации S)

    // 6а. Переключить корень каждой поглощаемой компоненты:
    //     parent[partner[i]] <- i  для всех представителей i
    //     (parent[i] = i для корня, поэтому пишем iota, а не parent)
    //     masked scatter: пишем только там, где index[i] == i

    rep_mask <- (index == iota)
    parent   <- scatter(iota, partner, first, mask=rep_mask)
    //          parent[partner[i]] <- i  для всех i, где rep_mask[i]

    // 6б. Сжатие путей методом pointer jumping

    repeat
        parent <- gather(parent, parent)    // parent[u] <- parent[parent[u]]
    until parent unchanged

    // Шаг 7. Удалить внутрикомпонентные рёбра

    s2 = fun i j -> parent[i] != parent[j]

    S <- select(S, s2)
}

return T
*)

type Error<'t1, 't2, 't3, 't4> =
    | NewFrontierCalculationProblem of LinearAlgebra.Error<'t1, 't2, 't1>
    | EdgesCalculationProblem of Vector.Error<'t1, 't2, 't3>
    | CEdgesCalculationProblem of Vector.Error<'t1, 't4, 't4>
    | IndexCalculationProblem of Vector.Error<'t1, 't4, 't4>

let mst (graph:Matrix.SparseMatrix<_>) =
    let op_add x y =
        match (x, y) with
        | Some(a), Some(b) -> Some(min a b)
        | Some(a), _
        | _, Some(a) -> Some(a)
        | _ -> None

    let op_mult (i,x) (row,col,w) =
        Some(w,row)
    
    let length = uint64 graph.nrows * 1UL<Vector.dataLength>
    let parent = Vector.init  length (fun i -> Some i)
    let iota = Vector.init  length (fun i -> Some (uint64 i))
    
    let rec inner (graph: Matrix.SparseMatrix<_>) (tree: Matrix.SparseMatrix<_>) parent =
        if graph.nvals > 0UL<nvals> then

            let edges = LinearAlgebra.vxmi_values op_add op_mult parent graph
            
            match edges with
            | Result.Failure(e) -> Result.Failure(EdgesCalculationProblem(e))
            | Result.Success(edges) ->
                let op_min x y =
                    match (x, y) with
                    | Some v, Some u -> if v < u then Some v else None
                    | Some v, _ -> Some v
                    | _ -> None

                let cedges = 
                    Vector.scatter (Vector.empty length) edges parent op_add

                match cedges with
                | Result.Failure(e) -> Result.Failure(CEdgesCalculationProblem(e))
                | Result.Success(cedges) ->

                    let t = Vector.gather cedges parent                    
                    let index = Vector.map2i t edges (fun i t e -> match (t,e) with |  Some v1, Some v2 when v1 = v2 -> Some i | _ -> None)
                    let index = Vector.scatter index index parent op_add
                    match index with 
                    | Result.Failure(e) -> Result.Failure(IndexCalculationProblem(e))
                    | Result.Success (index) ->
                        let index = Vector.gather index parent
                        
                        let filter i j = 
                            let i = uint64 i * 1UL<Vector.index>
                            let j = uint64 j * 1UL<Vector.index>
                            let edge = Vector.unsafeGet edges i
                            match edge with 
                            | Some(w,_to) -> 
                                let parent = Vector.unsafeGet parent j
                                let idx = Vector.unsafeGet index i
                                match parent,idx with
                                | Some p, Some idx -> uint64 p = uint64 _to && idx = i
                                | _ -> false
                            | None -> false
                        
                        let tree = 
                            Matrix.map2i tree graph (
                                fun i j t g -> 
                                    match (t,g) with
                                    | Some t, _ -> Some t
                                    | None, Some g when filter i j -> Some g
                                    | _ -> None)
                        



                        inner graph tree parent

                
        else
            Result.Success tree

    inner graph (Matrix.empty graph.nrows graph.ncols) parent


(*
let mst (graph:Matrix.SparseMatrix<'w>) =
    let n = uint64 graph.nrows

    let findRoot (parent: uint64 array) x =
        let rec loop (y: uint64) (visited: Set<uint64>) =
            if visited.Contains(y) then y
            else
                let p = parent.[int y]
                if p <> y then loop p (Set.add y visited) else y
        loop x Set.empty

    let union (parent: uint64 array) (rank: uint64 array) x y =
        let rootX = findRoot parent x
        let rootY = findRoot parent y
        if rootX <> rootY then
            if rank.[int rootX] < rank.[int rootY] then
                parent.[int rootX] <- rootY
            elif rank.[int rootX] > rank.[int rootY] then
                parent.[int rootY] <- rootX
            else
                parent.[int rootY] <- rootX
                rank.[int rootX] <- rank.[int rootX] + 1UL

    let rec inner (S: Matrix.SparseMatrix<'w>) (mstEdges: (uint64 * uint64 * 'w) list) (parent: uint64 array) (rank: uint64 array) (iterations: int) =
        if S.nvals > 0UL<nvals> && iterations < 20 then
            let edgesCL = Matrix.toCoordinateList S
            
            let edgesData = 
                edgesCL.list 
                |> List.map (fun (i, j, w) -> (uint64 i, uint64 j, w))

            let edgesWithCompInfo =
                edgesData
                |> List.map (fun (i, j, w) -> 
                    let rootJ = findRoot parent j
                    let rootI = findRoot parent i
                    (i, j, w, rootI, rootJ))

            let interCompEdges =
                edgesWithCompInfo
                |> List.filter (fun (i, j, w, rootI, rootJ) -> rootI <> rootJ)

            if interCompEdges.IsEmpty then
                let resultCL =
                    Matrix.CoordinateList(graph.nrows, graph.ncols,
                        List.map (fun (i, j, w) -> (i * 1UL<Matrix.rowindex>, j * 1UL<Matrix.colindex>, w)) mstEdges)
                Result.Success (Matrix.fromCoordinateList resultCL)
            else
                let compMinEdges =
                    interCompEdges
                    |> List.groupBy (fun (_, _, _, _, rootJ) -> rootJ)
                    |> List.map (fun (rootJ, edges) ->
                        let minEdge = List.minBy (fun (i, j, w, _, _) -> w) edges
                        (rootJ, minEdge))
                    |> Map.ofList

                let repToEdge =
                    compMinEdges
                    |> Map.toList
                    |> List.map (fun (destComp, edge) -> (destComp, edge))
                    |> List.filter (fun (_, (minI, minJ, _, _, _)) -> 
                        let canonicalI, canonicalJ = if minI < minJ then minI, minJ else minJ, minI
                        let srcRoot = findRoot parent canonicalI
                        let dstRoot = findRoot parent canonicalJ
                        srcRoot <> dstRoot)
                    |> List.distinctBy (fun (_, (minI, minJ, _, _, _)) -> 
                        if minI < minJ then (minI, minJ) else (minJ, minI))

                let newMstEdges =
                    repToEdge
                    |> List.collect (fun (_, (minI, minJ, minW, _, _)) -> 
                        [(minI, minJ, minW); (minJ, minI, minW)])
                    |> fun newEdges -> mstEdges @ newEdges

                let parentCopy = Array.copy parent
                let rankCopy = Array.copy rank

                for (_, (minI, minJ, _, _, _)) in repToEdge do
                    union parentCopy rankCopy minI minJ

                let rec pointerJump (parentArr: uint64 array) maxIter =
                    if maxIter = 0 then parentArr
                    else
                        let newParent = Array.copy parentArr
                        let mutable changed = false
                        for i in 0..int n-1 do
                            let root = findRoot parentArr parentArr.[i]
                            if root <> parentArr.[i] then
                                newParent.[i] <- root
                                changed <- true
                        if changed then pointerJump newParent (maxIter - 1) else newParent

                let parent''' = pointerJump parentCopy (int n)

                let s2 (i: uint64<Matrix.rowindex>) (j: uint64<Matrix.colindex>) (_: 'w) =
                    let ii = uint64 i
                    let jj = uint64 j
                    findRoot parent''' ii <> findRoot parent''' jj

                let S' =
                    Matrix.toCoordinateList S
                    |> fun cl ->
                        let filtered = cl.list |> List.filter (fun (i, j, w) -> s2 i j w)
                        Matrix.fromCoordinateList (Matrix.CoordinateList(cl.nrows, cl.ncols, filtered))

                inner S' newMstEdges parent''' rankCopy (iterations + 1)

        else
            let resultCL =
                Matrix.CoordinateList(graph.nrows, graph.ncols,
                    List.map (fun (i, j, w) -> (i * 1UL<Matrix.rowindex>, j * 1UL<Matrix.colindex>, w)) mstEdges)
            Result.Success (Matrix.fromCoordinateList resultCL)

    let parentInit = [| for i in 0UL..n-1UL -> i |]
    let rankInit = [| for i in 0UL..n-1UL -> 0UL |]
    inner graph [] parentInit rankInit 0
*)