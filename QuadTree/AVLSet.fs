namespace QuadTree.AVLSet

type AVLSet<'Value> =
    | Empty
    | Node of int * 'Value * AVLSet<'Value> * AVLSet<'Value>

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
            Node(max (height lln) (height rlnNew) + 1, vln, lln, rlnNew)
        | _ -> invalidArg "n" "Node cannot be rotated"

    let RRrotate n =
        match n with
        | Node(_, vn, ln, Node(_, vrn, lrn, rrn)) ->
            let lrnNew = Node(max (height ln) (height lrn) + 1, vn, ln, lrn)
            Node(max (height lrnNew) (height rrn) + 1, vrn, lrnNew, rrn)
        | _ -> invalidArg "n" "Node cannot be rotated"

    let LRrotate n =
        match n with
        | Node(hn, vn, ln, rn) ->
            let lnNew = RRrotate ln
            LLrotate(Node(max (height lnNew) (height rn) + 1, vn, lnNew, rn))
        | _ -> invalidArg "n" "Node cannot be rotated"

    let RLrotate n =
        match n with
        | Node(hn, vn, ln, rn) ->
            let rnNew = LLrotate rn
            RRrotate(Node(max (height ln) (height rnNew) + 1, vn, ln, rnNew))
        | _ -> invalidArg "n" "Node cannot be rotated"

    let balance ln rn v =
        let lnHeight = height ln
        let rnHeight = height rn
        let diff = lnHeight - rnHeight

        if diff >= 2 then
            match ln with
            | Empty -> invalidArg "rn" "left child is Empty but diff is lesser than 2"
            | Node(_, _, lln, rln) ->
                if height lln >= height rln then
                    LLrotate(Node(0, v, ln, rn))
                else
                    LRrotate(Node(0, v, ln, rn))
        elif diff <= -2 then
            match rn with
            | Empty -> invalidArg "rn" "right child is Empty but diff is greater than -2"
            | Node(_, _, lrn, rrn) ->
                if height lrn <= height rrn then
                    RRrotate(Node(0, v, ln, rn))
                else
                    RLrotate(Node(0, v, ln, rn))
        else
            Node(max lnHeight rnHeight + 1, v, ln, rn)

    let rec minNode n =
        match n with
        | Empty -> None
        | Node(_, v, Empty, rn) -> Some(v, rn)
        | Node(_, v, ln, rn) ->
            match minNode ln with
            | None -> None
            | Some(value, lnNew) -> Some(value, balance lnNew rn v)

    let rec insert value n =
        match n with
        | Empty -> Node(0, value, Empty, Empty)
        | Node(h, v, ln, rn) ->
            if value = v then
                n
            elif value < v then
                let lnNew = insert value ln
                balance lnNew rn v
            else
                let rnNew = insert value rn
                balance ln rnNew v

    let rec remove value n =
        match n with
        | Empty -> Empty
        | Node(h, v, ln, rn) ->
            if value = v then
                match ln, rn with
                | Empty, _ -> rn
                | _, Empty -> ln
                | _, _ ->
                    match minNode rn with
                    | None -> failwith "impossible error: rn is not Empty"
                    | Some(newValue, rnNew) -> balance ln rnNew newValue
            elif value < v then
                let lnNew = remove value ln
                balance lnNew rn v
            else
                let rnNew = remove value rn
                balance ln rnNew v

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

    let rec copy n =
        match n with
        | Empty -> Empty
        | Node(h, v, ln, rn) -> Node(h, v, copy ln, copy rn)

    let rec join left key right =
        let leftHeight = height left
        let rightHeight = height right
        let diff = leftHeight - rightHeight

        if abs diff <= 1 then
            Node(max leftHeight rightHeight + 1, key, left, right)
        elif diff >= 2 then
            match left with
            | Empty -> invalidArg "right" "left child is Empty but diff is lesser than 2"
            | Node(h, v, ln, rn) ->
                let rnNew = join rn key right
                balance ln rnNew v
        else
            match right with
            | Empty -> invalidArg "left" "right child is Empty but diff is greater than -2"
            | Node(h, v, ln, rn) ->
                let lnNew = join left key ln
                balance lnNew rn v

    let merge left right =
        match left, right with
        | Empty, _ -> right
        | _, Empty -> left
        | _, _ ->
            match minNode right with
            | None -> failwith "impossible error. right is not Empty"
            | Some(key, newRight) -> join left key newRight

    let rec split key n =
        match n with
        | Empty -> Empty, Empty, false
        | Node(_, v, ln, rn) ->
            if key = v then
                ln, rn, true
            elif key < v then
                let lesser, greater, wasFound = split key ln
                lesser, join greater v rn, wasFound
            else
                let lesser, greater, wasFound = split key rn
                join ln v lesser, greater, wasFound

module AVLSet =
    let empty = Empty

    let add value set = Tree.insert value set

    let delete value set = Tree.remove value set

    let contains value set = Tree.contains value set

    let copy set = Tree.copy set

    let rec union set1 set2 =
        let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2

        match maxSet, minSet with
        | Empty, _ -> minSet
        | _, Empty -> maxSet
        | Node(_, v, ln, rn), _ ->
            let lesser, greater, _ = Tree.split v minSet
            let leftUnion = union ln lesser
            let rightUnion = union rn greater
            Tree.join leftUnion v rightUnion

    let rec intersection set1 set2 =
        let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2

        match maxSet, minSet with
        | Empty, _ -> Empty
        | _, Empty -> Empty
        | Node(_, v, ln, rn), _ ->
            let lesser, greater, wasFound = Tree.split v minSet
            let leftInter = intersection ln lesser
            let rightInter = intersection rn greater

            if wasFound then
                Tree.join leftInter v rightInter
            else
                Tree.merge leftInter rightInter

    let rec difference minuendSet subtrahendSet =
        match minuendSet, subtrahendSet with
        | Empty, _ -> Empty
        | _, Empty -> minuendSet
        | Node(_, v, ln, rn), _ ->
            let lesser, greater, wasFound = Tree.split v subtrahendSet
            let leftDiff = difference ln lesser
            let rightDiff = difference rn greater

            if wasFound then
                Tree.merge leftDiff rightDiff
            else
                Tree.join leftDiff v rightDiff

    let rec symmDifference set1 set2 =
        let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2

        match maxSet, minSet with
        | Empty, _ -> minSet
        | _, Empty -> maxSet
        | Node(_, v, ln, rn), _ ->
            let lesser, greater, wasFound = Tree.split v minSet
            let leftSymm = symmDifference ln lesser
            let rightSymm = symmDifference rn greater

            if wasFound then
                Tree.merge leftSymm rightSymm
            else
                Tree.join leftSymm v rightSymm

    module Traversal =
        let union set1 set2 =
            let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2
            let unSet = Tree.copy maxSet
            Tree.traverse Tree.insert unSet minSet

        let intersection set1 set2 =
            let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2

            Tree.traverse
                (fun value set ->
                    if Tree.contains value maxSet then
                        Tree.insert value set
                    else
                        set)
                Empty
                minSet

        let difference minuendSet subtrahendSet =
            let diffSet = Tree.copy minuendSet
            Tree.traverse Tree.remove diffSet subtrahendSet

        let symmDifference set1 set2 =
            let maxSet, minSet = Tree.maxMinNodesByHeights set1 set2
            let symmSet = Tree.copy maxSet

            Tree.traverse
                (fun value set ->
                    if Tree.contains value maxSet then
                        Tree.remove value set
                    else
                        Tree.insert value set)
                symmSet
                minSet
