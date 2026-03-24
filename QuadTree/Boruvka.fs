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

type Error<'t1, 't2, 't3, 't4, 't5, 't6, 't7, 't8, 't9, 't10, 't11, 't12> =    
    | EdgesCalculationProblem of LinearAlgebra.Error<'t1, 't2, 't3>
    | CEdgesCalculationProblem of Vector.Error<'t4, 't5, 't6>
    | IndexCalculationProblem of Vector.Error<'t7, 't8, 't9>

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
