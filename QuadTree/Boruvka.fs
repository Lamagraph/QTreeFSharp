module Graph.Boruvka

open Common

type Error<'t1, 't2> =
    | NewFrontierCalculationProblem of LinearAlgebra.Error<'t1, 't2, 't1>
    | EdgesCalculationProblem of Vector.Error<'t1, 't1>
    | CEdgesCalculationProblem of Vector.Error<'t1, 't1>

(*
Вход: граф G = (V, E, w), матрица смежности S, n = |V|
Выход: множество рёбер МОД T

iota <- [0, 1, 2, ..., n-1]    // вектор индексов

parent[u] <- u    для всех u in 0..n-1
T <- ∅

while S ≠ ∅ do {

    // Шаг 1. Минимальное ребро каждой вершины
    //        mxv над полукольцом combMin:
    //        edge[u] = min{ (w, parent[v]) | (u,v,w) ∈ S }

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
    
    let rec inner (graph: Matrix.SparseMatrix<_>) (tree: Matrix.SparseMatrix<_>) =
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

                
        else
            Result.Success tree

    let frontier =
        Vector.CoordinateList((uint64 graph.ncols) * 1UL<Vector.dataLength>, [ 0UL * 1UL<Vector.index>, 0.0 ])
        |> Vector.fromCoordinateList

    inner frontier frontier 0
