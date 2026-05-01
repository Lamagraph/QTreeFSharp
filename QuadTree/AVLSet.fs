namespace QuadTree.AVLSet

open System.Threading.Tasks

type AVLTree<'Value> =
    | Empty
    | Node of int * 'Value * AVLTree<'Value> * AVLTree<'Value>

module Node =
    let height n =
        match n with
        | Empty -> -1
        | Node(h, _, _, _) -> h

    let value n =
        match n with
        | Empty -> failwith "Empty node has no value"
        | Node(_, v, _, _) -> v

    let leftChild n =
        match n with
        | Empty -> failwith "Empty node has no left child"
        | Node(_, _, ln, _) -> ln

    let rightChild n =
        match n with
        | Empty -> failwith "Empty node has no right child"
        | Node(_, _, _, rn) -> rn

    let maxMinNodesByHeights n1 n2 =
        if height n1 >= height n2 then n1, n2 else n2, n1

module Tree =
    let LLrotate n =
        let ln = Node.leftChild n
        let lln = Node.leftChild ln
        let rln = Node.rightChild ln
        let rn = Node.rightChild n
        let rlnNew = Node(max (Node.height rln) (Node.height rn) + 1, Node.value n, rln, rn)
        Node(max (Node.height lln) (Node.height rlnNew) + 1, Node.value ln, lln, rlnNew)

    let RRrotate n =
        let ln = Node.leftChild n
        let rn = Node.rightChild n
        let rrn = Node.rightChild rn
        let lrn = Node.leftChild rn
        let lrnNew = Node(max (Node.height ln) (Node.height lrn) + 1, Node.value n, ln, lrn)
        Node(max (Node.height lrnNew) (Node.height rrn) + 1, Node.value rn, lrnNew, rrn)

    let LRrotate n =
        let lnNew = RRrotate(Node.leftChild n)
        let rn = Node.rightChild n
        LLrotate(Node(max (Node.height lnNew) (Node.height rn) + 1, Node.value n, lnNew, rn))

    let RLrotate n =
        let rnNew = LLrotate(Node.rightChild n)
        let ln = Node.leftChild n
        RRrotate(Node(max (Node.height ln) (Node.height rnNew) + 1, Node.value n, ln, rnNew))

    let balance ln rn v =
        let lnHeight = Node.height ln
        let rnHeight = Node.height rn

        match lnHeight - rnHeight with
        | 2 ->
            let llnHeight = Node.height (Node.leftChild ln)
            let rlnHeight = Node.height (Node.rightChild ln)

            if llnHeight >= rlnHeight then
                LLrotate(Node(0, v, ln, rn))
            else
                LRrotate(Node(0, v, ln, rn))
        | -2 ->
            let lrnHeight = Node.height (Node.leftChild rn)
            let rrnHeight = Node.height (Node.rightChild rn)

            if lrnHeight <= rrnHeight then
                RRrotate(Node(0, v, ln, rn))
            else
                RLrotate(Node(0, v, ln, rn))
        | _ -> Node(max lnHeight rnHeight + 1, v, ln, rn)

    let rec minNode n =
        match n with
        | Empty -> failwith "minNode: cannot find minimum of an empty node"
        | Node(_, v, Empty, rn) -> v, rn
        | Node(_, v, ln, rn) ->
            let value, lnNew = minNode ln
            value, balance lnNew rn v

    let rec insert value n =
        match n with
        | Empty -> Node(0, value, Empty, Empty)
        | Node(h, v, ln, rn) ->
            match value with
            | value when value = v -> n
            | value when value < v ->
                let lnNew = insert value ln
                balance lnNew rn v
            | _ ->
                let rnNew = insert value rn
                balance ln rnNew v

    let rec remove value n =
        match n with
        | Empty -> Empty
        | Node(h, v, ln, rn) ->
            match value with
            | value when value = v ->
                match ln, rn with
                | Empty, _ -> rn
                | _, Empty -> ln
                | _, _ ->
                    let newValue, rnNew = minNode rn
                    balance ln rnNew newValue
            | value when value < v ->
                let lnNew = remove value ln
                balance lnNew rn v
            | _ ->
                let rnNew = remove value rn
                balance ln rnNew v

    [<TailCall>]
    let rec contains value n =
        match n with
        | Empty -> false
        | Node(h, v, ln, rn) ->
            match value with
            | value when value = v -> true
            | value when value < v -> contains value ln
            | _ -> contains value rn

    let rec traverse (func: 'A -> AVLTree<'B> -> AVLTree<'B>) nArg n =
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
        let leftHeight = Node.height left
        let rightHeight = Node.height right

        match leftHeight - rightHeight with
        | diff when abs diff <= 1 -> Node(max leftHeight rightHeight + 1, key, left, right)
        | diff when diff >= 2 ->
            match left with
            | Empty -> failwith "Unreacheable message 1"
            | Node(h, v, ln, rn) ->
                let rnNew = join rn key right
                balance ln rnNew v
        | _ ->
            match right with
            | Empty -> failwith "Unreacheable message 2"
            | Node(h, v, ln, rn) ->
                let lnNew = join left key ln
                balance lnNew rn v

    let merge left right =
        match left, right with
        | Empty, _ -> right
        | _, Empty -> left
        | _, _ ->
            let key, newRight = minNode right
            join left key newRight

    let rec split key n =
        match n with
        | Empty -> Empty, Empty, false
        | Node(_, v, ln, rn) ->
            match key with
            | key when key = v -> ln, rn, true
            | key when key < v ->
                let lesser, greater, wasFound = split key ln
                lesser, join greater v rn, wasFound
            | _ ->
                let lesser, greater, wasFound = split key rn
                join ln v lesser, greater, wasFound

module AVLSet =
    let empty = Empty

    let add value set = Tree.insert value set

    let delete value set = Tree.remove value set

    let contains value set = Tree.contains value set

    let copy set = Tree.copy set

    let rec union set1 set2 =
        let maxSet, minSet = Node.maxMinNodesByHeights set1 set2

        match maxSet, minSet with
        | Empty, _ -> minSet
        | _, Empty -> maxSet
        | Node(_, v, ln, rn), _ ->
            let lesser, greater, _ = Tree.split v minSet
            let leftUnion = union ln lesser
            let rightUnion = union rn greater
            Tree.join leftUnion v rightUnion

    let rec intersection set1 set2 =
        let maxSet, minSet = Node.maxMinNodesByHeights set1 set2

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
        let maxSet, minSet = Node.maxMinNodesByHeights set1 set2

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

    let unionTraversal set1 set2 =
        let maxSet, minSet = Node.maxMinNodesByHeights set1 set2
        let unSet = Tree.copy maxSet
        Tree.traverse Tree.insert unSet minSet

    let intersectionTraversal set1 set2 =
        let maxSet, minSet = Node.maxMinNodesByHeights set1 set2

        Tree.traverse
            (fun value set ->
                if Tree.contains value maxSet then
                    Tree.insert value set
                else
                    set)
            Empty
            minSet

    let differenceTraversal minuendSet subtrahendSet =
        let diffSet = Tree.copy minuendSet
        Tree.traverse Tree.remove diffSet subtrahendSet

    let symmDifferenceTraversal set1 set2 =
        let maxSet, minSet = Node.maxMinNodesByHeights set1 set2
        let symmSet = Tree.copy maxSet

        Tree.traverse
            (fun value set ->
                if Tree.contains value maxSet then
                    Tree.remove value set
                else
                    Tree.insert value set)
            symmSet
            minSet

    let rec parallelUnion (opts: ParallelOptions) set1 set2 =
        let maxSet, minSet = Node.maxMinNodesByHeights set1 set2

        match maxSet, minSet with
        | Empty, _ -> minSet
        | _, Empty -> maxSet
        | Node(_, v, ln, rn), _ ->
            let lesser, greater, _ = Tree.split v minSet
            let mutable leftUnion = Empty
            let mutable rightUnion = Empty

            Parallel.Invoke(
                opts,
                (fun () -> leftUnion <- parallelUnion opts ln lesser),
                (fun () -> rightUnion <- parallelUnion opts rn greater)
            )

            Tree.join leftUnion v rightUnion

    let rec parallelIntersection (opts: ParallelOptions) set1 set2 =
        let maxSet, minSet = Node.maxMinNodesByHeights set1 set2

        match maxSet, minSet with
        | Empty, _ -> Empty
        | _, Empty -> Empty
        | Node(_, v, ln, rn), _ ->
            let lesser, greater, wasFound = Tree.split v minSet
            let mutable leftInter = Empty
            let mutable rightInter = Empty

            Parallel.Invoke(
                opts,
                (fun () -> leftInter <- parallelIntersection opts ln lesser),
                (fun () -> rightInter <- parallelIntersection opts rn greater)
            )

            if wasFound then
                Tree.join leftInter v rightInter
            else
                Tree.merge leftInter rightInter

    let rec parallelDifference (opts: ParallelOptions) minuendSet subtrahendSet =
        match minuendSet, subtrahendSet with
        | Empty, _ -> Empty
        | _, Empty -> minuendSet
        | Node(_, v, ln, rn), _ ->
            let lesser, greater, wasFound = Tree.split v subtrahendSet
            let mutable leftDiff = Empty
            let mutable rightDiff = Empty

            Parallel.Invoke(
                opts,
                (fun () -> leftDiff <- parallelDifference opts ln lesser),
                (fun () -> rightDiff <- parallelDifference opts rn greater)
            )

            if wasFound then
                Tree.merge leftDiff rightDiff
            else
                Tree.join leftDiff v rightDiff

    let rec parallelSymmDifference (opts: ParallelOptions) set1 set2 =
        let maxSet, minSet = Node.maxMinNodesByHeights set1 set2

        match maxSet, minSet with
        | Empty, _ -> minSet
        | _, Empty -> maxSet
        | Node(_, v, ln, rn), _ ->
            let lesser, greater, wasFound = Tree.split v minSet
            let mutable leftSymm = Empty
            let mutable rightSymm = Empty

            Parallel.Invoke(
                opts,
                (fun () -> leftSymm <- parallelSymmDifference opts ln lesser),
                (fun () -> rightSymm <- parallelSymmDifference opts rn greater)
            )

            if wasFound then
                Tree.merge leftSymm rightSymm
            else
                Tree.join leftSymm v rightSymm
