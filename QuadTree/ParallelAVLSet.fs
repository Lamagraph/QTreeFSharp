namespace QuadTree.AVLSet.Parallel

open QuadTree.AVLSet
open Result

/// <summary>
/// Parallel union of two AVL sets.
/// </summary>
/// <param name="threads">
/// Optional thread limit:
/// - None: Auto-detect (uses all available CPU cores via System.Environment.ProcessorCount).
/// - Some(x): Hard limit to x threads (useful for benchmarking and resource control).
/// </param>

module ParallelAVLSet =
    let [<Literal>] HeightThreshold = 10

    let rec unionAsync threads set1 set2 =
        async {
            let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2

            match maxSet, minSet with
            | Empty, _ -> return Ok minSet
            | _, Empty -> return Ok maxSet
            | Node(h, v, ln, rn), _ ->

                if h < HeightThreshold then
                    return AVLSet.union maxSet minSet
                else
                    match Tree.split v minSet with
                    | Error err -> return Error err
                    | Ok(lesser, greater, _) ->

                        let limit = defaultArg threads System.Environment.ProcessorCount

                        let left = unionAsync threads ln lesser
                        let right = unionAsync threads rn greater

                        let! results = Async.Parallel([| left; right |], limit)

                        let finalResult =
                            resultM {
                                let! leftUnion = results[0]
                                let! rightUnion = results[1]

                                return! Tree.join leftUnion v rightUnion
                            }

                        return finalResult
        }

    let rec intersectionAsync threads set1 set2 =
        async {
            let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2

            match maxSet, minSet with
            | Empty, _ -> return Ok Empty
            | _, Empty -> return Ok Empty
            | Node(h, v, ln, rn), _ ->

                if h < HeightThreshold then
                    return AVLSet.intersection maxSet minSet
                else
                    match Tree.split v minSet with
                    | Error err -> return Error err
                    | Ok(lesser, greater, wasFound) ->

                        let limit = defaultArg threads System.Environment.ProcessorCount

                        let left = intersectionAsync threads ln lesser
                        let right = intersectionAsync threads rn greater

                        let! results = Async.Parallel([| left; right |], limit)

                        let finalResult =
                            resultM {
                                let! leftInter = results[0]
                                let! rightInter = results[1]

                                return!
                                    if wasFound then
                                        Tree.join leftInter v rightInter
                                    else
                                        Tree.merge leftInter rightInter
                            }

                        return finalResult
        }

    let rec differenceAsync threads minuendSet subtrahendSet =
        async {
            match minuendSet, subtrahendSet with
            | Empty, _ -> return Ok Empty
            | _, Empty -> return Ok minuendSet
            | Node(h, v, ln, rn), _ ->

                if h < HeightThreshold then
                    return AVLSet.difference minuendSet subtrahendSet
                else
                    match Tree.split v subtrahendSet with
                    | Error err -> return Error err
                    | Ok(lesser, greater, wasFound) ->

                        let limit = defaultArg threads System.Environment.ProcessorCount

                        let left = differenceAsync threads ln lesser
                        let right = differenceAsync threads rn greater

                        let! results = Async.Parallel([| left; right |], limit)

                        let finalResult =
                            resultM {
                                let! leftDiff = results[0]
                                let! rightDiff = results[1]

                                return!
                                    if wasFound then
                                        Tree.merge leftDiff rightDiff
                                    else
                                        Tree.join leftDiff v rightDiff
                            }

                        return finalResult
        }

    let rec symmDifferenceAsync threads set1 set2 =
        async {
            let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2

            match maxSet, minSet with
            | Empty, _ -> return Ok minSet
            | _, Empty -> return Ok maxSet
            | Node(h, v, ln, rn), _ ->

                if h < HeightThreshold then
                    return AVLSet.symmDifference maxSet minSet
                else
                    match Tree.split v minSet with
                    | Error err -> return Error err
                    | Ok(lesser, greater, wasFound) ->

                        let limit = defaultArg threads System.Environment.ProcessorCount

                        let left = symmDifferenceAsync threads ln lesser
                        let right = symmDifferenceAsync threads rn greater

                        let! results = Async.Parallel([| left; right |], limit)

                        let finalResult =
                            resultM {
                                let! leftSymm = results[0]
                                let! rightSymm = results[1]

                                return!
                                    if wasFound then
                                        Tree.merge leftSymm rightSymm
                                    else
                                        Tree.join leftSymm v rightSymm
                            }

                        return finalResult
        }

    let union threads t1 t2 =
        unionAsync threads t1 t2 |> Async.RunSynchronously

    let intersection threads t1 t2 =
        intersectionAsync threads t1 t2 |> Async.RunSynchronously

    let difference threads t1 t2 =
        differenceAsync threads t1 t2 |> Async.RunSynchronously

    let symmDifference threads t1 t2 =
        symmDifferenceAsync threads t1 t2 |> Async.RunSynchronously
