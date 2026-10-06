//The following sources were used as a reference: 'Faster, Simpler Red-Black Trees' and Data/Set/RBTree.hs.
namespace QuadTree.RBSet

open Result

type RBSetError = EmptyNodeWasNotExpected

module Tree =
    type Color =
        | Red
        | Black

    type Tree<'T> =
        | Empty
        | Node of color: Color * left: Tree<'T> * value: 'T * right: Tree<'T>

    type private Condition<'T> =
        | Done of 'T
        | ToDo of 'T

    let private justTree resultTree =
        match resultTree with
        | Done t
        | ToDo t -> t

    let blackenRoot tree =
        match tree with
        | Node(Red, a, x, b) -> Node(Black, a, x, b)
        | _ -> tree

    let rec private getBlackHeight tree =
        match tree with
        | Empty -> 0
        | Node(Red, l, _, _) -> getBlackHeight l
        | Node(Black, l, _, _) -> 1 + (getBlackHeight l)

    let rec contains tree v =
        match tree with
        | Empty -> false
        | Node(_, left, value, right) ->
            if value > v then contains left v
            elif value < v then contains right v
            else true

    let private balance tree =
        match tree with
        | Node(Black, Node(Red, Node(Red, a, x, b), y, c), z, d)
        | Node(Black, Node(Red, a, x, Node(Red, b, y, c)), z, d)
        | Node(Black, a, x, Node(Red, Node(Red, b, y, c), z, d))
        | Node(Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) ->
            ToDo(Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d)))
        | Node(Black, a, x, b) as n -> Done(n)
        | _ -> ToDo(tree)

    let insert tree v =
        let rec insertRec tree v =
            match tree with
            | Empty -> ToDo(Node(Red, Empty, v, Empty))
            | Node(color, left, value, right) ->
                if value > v then
                    let newLeft = insertRec left v

                    match newLeft with
                    | Done nl -> Done(Node(color, nl, value, right))
                    | ToDo nl -> balance (Node(color, nl, value, right))
                elif value < v then
                    let newRight = insertRec right v

                    match newRight with
                    | Done nr -> Done(Node(color, left, value, nr))
                    | ToDo nr -> balance (Node(color, left, value, nr))
                else
                    Done(tree)

        let newTree = insertRec tree v
        newTree |> justTree |> blackenRoot |> Ok

    let delete tree v =
        let blacken tree =
            match tree with
            | Node(Red, a, x, b) -> Done(Node(Black, a, x, b))
            | _ -> ToDo tree

        let balanceDel tree =
            match tree with
            | Node(color, Node(Red, Node(Red, a, x, b), y, c), z, d)
            | Node(color, Node(Red, a, x, Node(Red, b, y, c)), z, d)
            | Node(color, a, x, Node(Red, Node(Red, b, y, c), z, d))
            | Node(color, a, x, Node(Red, b, y, Node(Red, c, z, d))) ->
                Done(Node(color, Node(Black, a, x, b), y, Node(Black, c, z, d)))
            | _ -> blacken tree

        let rec eqL tree =
            resultM {
                match tree with
                | Node(color, a, x, Node(Black, b, y, c)) -> return balanceDel (Node(color, a, x, Node(Red, b, y, c)))
                | Node(color, a, x, Node(Red, b, y, c)) ->
                    let! newLeft = eqL (Node(Red, a, x, b))

                    match newLeft with
                    | Done nl -> return Done(Node(Black, nl, y, c))
                    | ToDo nl -> return ToDo(Node(Black, nl, y, c))
                | _ -> return! Error EmptyNodeWasNotExpected
            }

        let rec eqR tree =
            resultM {
                match tree with
                | Node(color, Node(Black, a, x, b), y, c) -> return balanceDel (Node(color, Node(Red, a, x, b), y, c))
                | Node(color, Node(Red, a, x, b), y, c) ->
                    let! newRight = eqR (Node(Red, b, y, c))

                    match newRight with
                    | Done nr -> return Done(Node(Black, a, x, nr))
                    | ToDo nr -> return ToDo(Node(Black, a, x, nr))
                | _ -> return! Error EmptyNodeWasNotExpected
            }

        let delCur tree =
            resultM {
                let rec delMin tree =
                    resultM {
                        match tree with
                        | Node(Red, Empty, x, b) -> return Done b, x
                        | Node(Black, Empty, x, b) -> return blacken b, x
                        | Node(color, a, x, b) ->
                            let! an, min = delMin a

                            match an with
                            | Done t -> return Done(Node(color, t, x, b)), min
                            | ToDo t ->
                                let! t' = eqL (Node(color, t, x, b))
                                return t', min
                        | _ -> return! Error EmptyNodeWasNotExpected
                    }

                match tree with
                | Node(Red, a, y, Empty) -> return Done a
                | Node(Black, a, x, Empty) -> return blacken a
                | Node(color, a, x, b) ->
                    let! bn, min = delMin b

                    match bn with
                    | Done t -> return Done(Node(color, a, min, t))
                    | ToDo t -> return! eqR (Node(color, a, min, t))
                | _ -> return! Error EmptyNodeWasNotExpected
            }

        let rec deleteRec tree v =
            resultM {
                match tree with
                | Empty -> return Done(Empty)
                | Node(color, left, value, right) ->
                    if value > v then
                        let! newLeft = deleteRec left v

                        match newLeft with
                        | Done nl -> return Done(Node(color, nl, value, right))
                        | ToDo nl -> return! eqL (Node(color, nl, value, right))
                    elif value < v then
                        let! newRight = deleteRec right v

                        match newRight with
                        | Done nr -> return Done(Node(color, left, value, nr))
                        | ToDo nr -> return! eqR (Node(color, left, value, nr))
                    else
                        return! delCur tree
            }

        resultM {
            let! t = deleteRec tree v
            return t |> justTree |> blackenRoot
        }

    let join t1 g t2 =
        let rec joinLT t1 g t2 targetHeight currentHeight =
            resultM {
                if targetHeight = currentHeight then
                    return Node(Red, t1, g, t2)
                else
                    match t2 with
                    | Node(Red, l, x, r) ->
                        let! newLeft = joinLT t1 g l targetHeight currentHeight
                        return Node(Red, newLeft, x, r) |> balance |> justTree
                    | Node(Black, l, x, r) ->
                        let! newLeft = joinLT t1 g l targetHeight (currentHeight - 1)
                        return Node(Black, newLeft, x, r) |> balance |> justTree
                    | _ -> return! Error EmptyNodeWasNotExpected
            }

        let rec joinRT t1 g t2 targetHeight currentHeight =
            resultM {
                if targetHeight = currentHeight then
                    return Node(Red, t1, g, t2)
                else
                    match t1 with
                    | Node(Red, l, x, r) ->
                        let! newRight = joinRT r g t2 targetHeight currentHeight
                        return Node(Red, l, x, newRight) |> balance |> justTree
                    | Node(Black, l, x, r) ->
                        let! newRight = joinRT r g t2 targetHeight (currentHeight - 1)
                        return Node(Black, l, x, newRight) |> balance |> justTree
                    | _ -> return! Error EmptyNodeWasNotExpected
            }

        let h1 = getBlackHeight t1
        let h2 = getBlackHeight t2

        resultM {
            if h1 = 0 then
                return! insert t2 g
            elif h2 = 0 then
                return! insert t1 g
            elif h1 < h2 then
                let! t = joinLT t1 g t2 h1 h2
                return blackenRoot t
            else if h1 > h2 then
                let! t = joinRT t1 g t2 h2 h1
                return blackenRoot t
            else
                return Node(Black, t1, g, t2)
        }

    let merge t1 t2 =
        resultM {
            match t1, t2 with
            | Empty, t -> return t
            | t, Empty -> return t
            | _, _ ->
                let rec extractMin tree =
                    match tree with
                    | Node(_, Empty, x, _) -> x
                    | Node(_, l, _, _) -> extractMin l
                    | Empty -> failwith "extractMin: empty tree"

                let minVal = extractMin t2
                let! t2Rest = delete t2 minVal
                return! join (blackenRoot t1) minVal (blackenRoot t2Rest)
        }

    let rec split kx tree =
        resultM {
            match tree with
            | Empty -> return Empty, Empty
            | Node(_, l, x, r) ->
                if kx < x then
                    let! lt, gt = split kx l
                    let! t = join gt x (blackenRoot r)
                    return lt, t
                else if kx > x then
                    let! lt, gt = split kx r
                    let! t = join (blackenRoot l) x lt
                    return t, gt
                else
                    return blackenRoot l, blackenRoot r
        }

module RBSet =
    open Tree

    type RBSet<'T> = Tree<'T>
    let empty = Empty

    let add value set = Tree.insert set value

    let delete value set = Tree.delete set value

    let contains value set = Tree.contains set value

    let rec union set1 set2 =
        resultM {
            match set1 with
            | Empty -> return blackenRoot set2
            | _ ->
                match set2 with
                | Empty -> return blackenRoot set1
                | Node(_, l, x, r) ->
                    let! l', r' = split x set1
                    let! tl = union l' l
                    let! tr = union r' r
                    return! join (blackenRoot tl) x (blackenRoot tr)
        }

    let rec intersection set1 set2 =
        resultM {
            match set1 with
            | Empty -> return Empty
            | _ ->
                match set2 with
                | Empty -> return Empty
                | Node(_, l, x, r) ->
                    let! l', r' = split x set1
                    let! tl = intersection l' l
                    let! tr = intersection r' r

                    if Tree.contains set1 x then
                        return! join (blackenRoot tl) x (blackenRoot tr)
                    else
                        return! merge (blackenRoot tl) (blackenRoot tr)
        }

    let rec difference set1 set2 =
        resultM {
            match set1 with
            | Empty -> return Empty
            | _ ->
                match set2 with
                | Empty -> return blackenRoot set1
                | Node(_, l, x, r) ->
                    let! l', r' = split x set1
                    let! tl = difference l' l
                    let! tr = difference r' r
                    return! merge (blackenRoot tl) (blackenRoot tr)
        }
