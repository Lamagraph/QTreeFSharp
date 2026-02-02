module Graph.BFS

// let bfs_general op_add graph startVertices =
//     let rec inner frontier visited =
//         if Vector.nvals frontier > 0 then
//             let new_frontier = LinearAlgebra.vxm frontier graph
//             let frontier = Vector.mask new_frontier visited (fun x -> x.IsNone)
//             let visited = Vector.map2 visited new_frontier op_add
//             inner frontier visited
//         else
//             visited

//     inner startVertices startVertices
