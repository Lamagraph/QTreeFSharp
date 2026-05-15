namespace QuadTree.AVLSet.Parallel

open QuadTree.AVLSet

/// <summary>
/// Parallel union of two AVL sets.
/// </summary>
/// <param name="threads">
/// Optional thread limit:
/// - None: Auto-detect (uses all available CPU cores via System.Environment.ProcessorCount).
/// - Some(x): Hard limit to x threads (useful for benchmarking and resource control).
/// </param>

module ParallelAVLSet =
    let rec unionAsync threads set1 set2 =
        async {
            let maxSet, minSet = Node.maxMinNodesByHeights set1 set2

            match maxSet, minSet with
            | Empty, _ -> return minSet
            | _, Empty -> return maxSet
            | Node(_, v, ln, rn), _ ->
                let lesser, greater, _ = Tree.split v minSet

                let limit = defaultArg threads System.Environment.ProcessorCount

                let left = unionAsync threads ln lesser
                let right = unionAsync threads rn greater

                let! results = Async.Parallel([| left; right |], limit)
                let leftUnion, rightUnion = results[0], results[1]

                return Tree.join leftUnion v rightUnion
        }

    let rec intersectionAsync threads set1 set2 =
        async {
            let maxSet, minSet = Node.maxMinNodesByHeights set1 set2

            match maxSet, minSet with
            | Empty, _ -> return Empty
            | _, Empty -> return Empty
            | Node(_, v, ln, rn), _ ->
                let lesser, greater, wasFound = Tree.split v minSet

                let limit = defaultArg threads System.Environment.ProcessorCount

                let left = intersectionAsync threads ln lesser
                let right = intersectionAsync threads rn greater

                let! results = Async.Parallel([| left; right |], limit)
                let leftInter, rightInter = results[0], results[1]

                return
                    if wasFound then
                        Tree.join leftInter v rightInter
                    else
                        Tree.merge leftInter rightInter
        }

    let rec differenceAsync threads minuendSet subtrahendSet =
        async {
            match minuendSet, subtrahendSet with
            | Empty, _ -> return Empty
            | _, Empty -> return minuendSet
            | Node(_, v, ln, rn), _ ->
                let lesser, greater, wasFound = Tree.split v subtrahendSet

                let limit = defaultArg threads System.Environment.ProcessorCount

                let left = differenceAsync threads ln lesser
                let right = differenceAsync threads rn greater

                let! results = Async.Parallel([| left; right |], limit)
                let leftDiff, rightDiff = results[0], results[1]

                return
                    if wasFound then
                        Tree.merge leftDiff rightDiff
                    else
                        Tree.join leftDiff v rightDiff
        }

    let rec symmDifferenceAsync threads set1 set2 =
        async {
            let maxSet, minSet = Node.maxMinNodesByHeights set1 set2

            match maxSet, minSet with
            | Empty, _ -> return minSet
            | _, Empty -> return maxSet
            | Node(_, v, ln, rn), _ ->
                let lesser, greater, wasFound = Tree.split v minSet

                let limit = defaultArg threads System.Environment.ProcessorCount

                let left = symmDifferenceAsync threads ln lesser
                let right = symmDifferenceAsync threads rn greater

                let! results = Async.Parallel([| left; right |], limit)
                let leftSymm, rightSymm = results[0], results[1]

                return
                    if wasFound then
                        Tree.merge leftSymm rightSymm
                    else
                        Tree.join leftSymm v rightSymm
        }

    let union threads t1 t2 =
        unionAsync threads t1 t2 |> Async.RunSynchronously

    let intersection threads t1 t2 =
        intersectionAsync threads t1 t2 |> Async.RunSynchronously

    let difference threads t1 t2 =
        differenceAsync threads t1 t2 |> Async.RunSynchronously

    let symmDifference threads t1 t2 =
        symmDifferenceAsync threads t1 t2 |> Async.RunSynchronously
