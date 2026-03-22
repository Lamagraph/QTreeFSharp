module Graph.Boruvka

open Common

type Error<'t1, 't2> =
    | NewFrontierCalculationProblem of LinearAlgebra.Error<'t1, 't2, 't1>
    | FrontierCalculationProblem of Vector.Error<'t1, 't1>
    | VisitedCalculationProblem of Vector.Error<'t1, 't1>

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

let mst graph =
    let op_add x y =
        match (x, y) with
        | Some(u), Some(v) -> Some(min u v)
        | Some(v), _
        | _, Some(v) -> Some(v)
        | _ -> None

    let op_mult x y =
        match (x, y) with
        | Some(u), Some(v) -> Some(u + v)
        | _ -> None

    let rec inner (frontier: Vector.SparseVector<_>) (visited: Vector.SparseVector<_>) iter_num =
        if frontier.nvals > 0UL<nvals> && iter_num <= int frontier.length then

            let new_frontier = LinearAlgebra.vxm op_add op_mult frontier graph

            match new_frontier with
            | Result.Failure(e) -> Result.Failure(NewFrontierCalculationProblem(e))
            | Result.Success(new_frontier) ->
                let op_min x y =
                    match (x, y) with
                    | Some v, Some u -> if v < u then Some v else None
                    | Some v, _ -> Some v
                    | _ -> None

                let frontier = Vector.map2 new_frontier visited op_min

                match frontier with
                | Result.Failure(e) -> Result.Failure(FrontierCalculationProblem(e))
                | Result.Success(frontier) ->

                    let visited = Vector.map2 visited frontier op_add

                    match visited with
                    | Result.Failure(e) -> Result.Failure(VisitedCalculationProblem(e))
                    | Result.Success(visited) -> inner frontier visited (iter_num + 1)
        else
            Result.Success visited

    let frontier =
        Vector.CoordinateList((uint64 graph.ncols) * 1UL<Vector.dataLength>, [ startVertex * 1UL<Vector.index>, 0.0 ])
        |> Vector.fromCoordinateList

    inner frontier frontier 0
