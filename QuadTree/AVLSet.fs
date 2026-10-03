namespace QuadTree.AVLSet

open Result

type AVLSet<'Value> =
    | Empty
    | Node of int * 'Value * AVLSet<'Value> * AVLSet<'Value>

type AVLSetError =
    | RotationError
    | InvalidHeightOfNode
    | EmptyNodeWasNotExpected

module Tree =
    let height n =
        match n with
        | Empty -> -1
        | Node(h, _, _, _) -> h

    let maxMinNodesByHeights n1 n2 =
        match n1, n2 with
        | Empty, _ -> n2, n1
        | _, Empty -> n1, n2
        | Node(h1, _, _, _), Node(h2, _, _, _) -> if h1 >= h2 then n1, n2 else n2, n1

    let LLrotate n =
        match n with
        | Node(_, vn, Node(_, vln, lln, rln), rn) ->
            let rlnNew = Node(max (height rln) (height rn) + 1, vn, rln, rn)
            Ok(Node(max (height lln) (height rlnNew) + 1, vln, lln, rlnNew))
        | _ -> Error RotationError

    let RRrotate n =
        match n with
        | Node(_, vn, ln, Node(_, vrn, lrn, rrn)) ->
            let lrnNew = Node(max (height ln) (height lrn) + 1, vn, ln, lrn)
            Ok(Node(max (height lrnNew) (height rrn) + 1, vrn, lrnNew, rrn))
        | _ -> Error RotationError


    let LRrotate n =
        resultM {
            match n with
            | Node(hn, vn, ln, rn) ->
                let! lnNew = RRrotate ln
                return! LLrotate(Node(max (height lnNew) (height rn) + 1, vn, lnNew, rn))
            | _ -> return! Error RotationError
        }

    let RLrotate n =
        resultM {
            match n with
            | Node(hn, vn, ln, rn) ->
                let! rnNew = LLrotate rn
                return! RRrotate(Node(max (height ln) (height rnNew) + 1, vn, ln, rnNew))
            | _ -> return! Error RotationError
        }

    let balance ln rn v =
        let lnHeight = height ln
        let rnHeight = height rn
        let diff = lnHeight - rnHeight

        if diff >= 2 then
            match ln with
            | Empty -> Error InvalidHeightOfNode
            | Node(_, _, lln, rln) ->
                if height lln >= height rln then
                    LLrotate(Node(0, v, ln, rn))
                else
                    LRrotate(Node(0, v, ln, rn))
        elif diff <= -2 then
            match rn with
            | Empty -> Error InvalidHeightOfNode
            | Node(_, _, lrn, rrn) ->
                if height lrn <= height rrn then
                    RRrotate(Node(0, v, ln, rn))
                else
                    RLrotate(Node(0, v, ln, rn))
        else
            Ok(Node(max lnHeight rnHeight + 1, v, ln, rn))

    let rec minNode n =
        resultM {
            match n with
            | Empty -> return! Error EmptyNodeWasNotExpected
            | Node(_, v, Empty, rn) -> return! Ok(v, rn)
            | Node(_, v, ln, rn) ->
                let! value, lnNew = minNode ln
                let! balanceRes = balance lnNew rn v
                return value, balanceRes
        }

    let rec insert value n =
        resultM {
            match n with
            | Empty -> return Node(0, value, Empty, Empty)
            | Node(h, v, ln, rn) ->
                if value = v then
                    return n
                elif value < v then
                    let! lnNew = insert value ln
                    return! balance lnNew rn v
                else
                    let! rnNew = insert value rn
                    return! balance ln rnNew v
        }

    let rec remove value n =
        resultM {
            match n with
            | Empty -> return Empty
            | Node(h, v, ln, rn) ->
                if value = v then
                    match ln, rn with
                    | Empty, _ -> return rn
                    | _, Empty -> return ln
                    | _, _ ->
                        let! newValue, rnNew = minNode rn
                        return! balance ln rnNew newValue
                elif value < v then
                    let! lnNew = remove value ln
                    return! balance lnNew rn v
                else
                    let! rnNew = remove value rn
                    return! balance ln rnNew v
        }

    [<TailCall>]
    let rec contains value n =
        match n with
        | Empty -> false
        | Node(h, v, ln, rn) ->
            if value = v then true
            elif value < v then contains value ln
            else contains value rn

    let rec traverse (func: 'A -> AVLSet<'B> -> AVLSet<'B>) nArg n =
        match n with
        | Empty -> nArg
        | Node(_, v, ln, rn) ->
            let newNArg = traverse func nArg ln
            let newNArg2 = func v newNArg
            traverse func newNArg2 rn

    let rec traverseRes (func: 'A -> AVLSet<'B> -> Result<AVLSet<'B>, AVLSetError>) nArg n =
        resultM {
            match n with
            | Empty -> return nArg
            | Node(_, v, ln, rn) ->
                let! newNArg = traverseRes func nArg ln
                let! newNArg2 = func v newNArg
                return! traverseRes func newNArg2 rn
        }

    let rec copy n =
        match n with
        | Empty -> Empty
        | Node(h, v, ln, rn) -> Node(h, v, copy ln, copy rn)

    let rec join left key right =
        let leftHeight = height left
        let rightHeight = height right
        let diff = leftHeight - rightHeight

        resultM {
            if abs diff <= 1 then
                return Node(max leftHeight rightHeight + 1, key, left, right)
            elif diff >= 2 then
                match left with
                | Empty -> return! Error InvalidHeightOfNode
                | Node(h, v, ln, rn) ->
                    let! rnNew = join rn key right
                    return! balance ln rnNew v
            else
                match right with
                | Empty -> return! Error InvalidHeightOfNode
                | Node(h, v, ln, rn) ->
                    let! lnNew = join left key ln
                    return! balance lnNew rn v
        }

    let merge left right =
        resultM {
            match left, right with
            | Empty, _ -> return right
            | _, Empty -> return left
            | _, _ ->
                let! key, newRight = minNode right
                return! join left key newRight
        }

    let rec split key n =
        resultM {
            match n with
            | Empty -> return Empty, Empty, false
            | Node(_, v, ln, rn) ->
                if key = v then
                    return ln, rn, true
                elif key < v then
                    let! lesser, greater, wasFound = split key ln
                    let! joinRes = join greater v rn
                    return lesser, joinRes, wasFound
                else
                    let! lesser, greater, wasFound = split key rn
                    let! joinRes = join ln v lesser
                    return joinRes, greater, wasFound
        }

module AVLSet =
    let empty = Empty

    let add value set = Tree.insert value set

    let delete value set = Tree.remove value set

    let contains value set = Tree.contains value set

    let copy set = Tree.copy set

    let rec union set1 set2 =
        let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2

        resultM {
            match maxSet, minSet with
            | Empty, _ -> return minSet
            | _, Empty -> return maxSet
            | Node(_, v, ln, rn), _ ->
                let! lesser, greater, _ = Tree.split v minSet
                let! leftUnion = union ln lesser
                let! rightUnion = union rn greater
                return! Tree.join leftUnion v rightUnion
        }

    let rec intersection set1 set2 =
        let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2

        resultM {
            match maxSet, minSet with
            | Empty, _ -> return Empty
            | _, Empty -> return Empty
            | Node(_, v, ln, rn), _ ->
                let! lesser, greater, wasFound = Tree.split v minSet
                let! leftInter = intersection ln lesser
                let! rightInter = intersection rn greater

                return!
                    if wasFound then
                        Tree.join leftInter v rightInter
                    else
                        Tree.merge leftInter rightInter
        }

    let rec difference minuendSet subtrahendSet =
        resultM {
            match minuendSet, subtrahendSet with
            | Empty, _ -> return Empty
            | _, Empty -> return minuendSet
            | Node(_, v, ln, rn), _ ->
                let! lesser, greater, wasFound = Tree.split v subtrahendSet
                let! leftDiff = difference ln lesser
                let! rightDiff = difference rn greater

                return!
                    if wasFound then
                        Tree.merge leftDiff rightDiff
                    else
                        Tree.join leftDiff v rightDiff
        }

    let rec symmDifference set1 set2 =
        let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2

        resultM {
            match maxSet, minSet with
            | Empty, _ -> return minSet
            | _, Empty -> return maxSet
            | Node(_, v, ln, rn), _ ->
                let! lesser, greater, wasFound = Tree.split v minSet
                let! leftSymm = symmDifference ln lesser
                let! rightSymm = symmDifference rn greater

                return!
                    if wasFound then
                        Tree.merge leftSymm rightSymm
                    else
                        Tree.join leftSymm v rightSymm
        }

    module Traversal =
        let union set1 set2 =
            let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2
            let unSet = Tree.copy maxSet
            Tree.traverseRes Tree.insert unSet minSet

        let intersection set1 set2 =
            let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2

            Tree.traverseRes
                (fun value set ->
                    if Tree.contains value maxSet then
                        Tree.insert value set
                    else
                        Ok set)
                Empty
                minSet

        let difference minuendSet subtrahendSet =
            let diffSet = Tree.copy minuendSet
            Tree.traverseRes Tree.remove diffSet subtrahendSet

        let symmDifference set1 set2 =
            let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2
            let symmSet = Tree.copy maxSet

            Tree.traverseRes
                (fun value set ->
                    if Tree.contains value maxSet then
                        Tree.remove value set
                    else
                        Tree.insert value set)
                symmSet
                minSet
