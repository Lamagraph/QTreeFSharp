namespace QuadTree.Tests.AVLSet

open Xunit
open FsUnit.Xunit
open FsCheck.Xunit
open QuadTree.AVLSet
open QuadTree.AVLSet.Parallel

module SetTests =
    let rec isSetValid n mn mx =
        match n with
        | Empty -> true
        | Node(h, v, ln, rn) ->
            let isInBounds =
                mn |> Option.forall (fun mn -> v > mn) && mx |> Option.forall (fun mx -> v < mx)

            let lnHeight = Node.height ln
            let rnHeight = Node.height rn

            isInBounds
            && h = (max lnHeight rnHeight + 1)
            && abs (lnHeight - rnHeight) <= 1
            && isSetValid ln mn (Some v)
            && isSetValid rn (Some v) mx

    let rec advancedContains (condition: 'A -> bool -> bool) setOfValues targetSet =
        match setOfValues with
        | Empty -> true
        | Node(_, v, ln, rn) ->
            let lesser, greater, wasFound = Tree.split v targetSet

            condition v wasFound
            && advancedContains condition ln lesser
            && advancedContains condition rn greater

    [<Fact>]
    let ``Empty tree insertion`` () =
        let resultSet = Empty |> AVLSet.add 15

        let correctSet = Node(0, 15, Empty, Empty)

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Duplicate element insertion`` () =
        let resultSet = Node(0, 15, Empty, Empty) |> AVLSet.add 15

        let correctSet = Node(0, 15, Empty, Empty)

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Insertion without rotation`` () =
        let resultSet = Node(1, 15, Node(0, 10, Empty, Empty), Empty) |> AVLSet.add 20

        let correctSet = Node(1, 15, Node(0, 10, Empty, Empty), Node(0, 20, Empty, Empty))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Insertion with height update`` () =
        let resultSet =
            Node(2, 15, Node(1, 10, Node(0, 7, Empty, Empty), Empty), Node(0, 20, Empty, Empty))
            |> AVLSet.add 13

        let correctSet =
            Node(2, 15, Node(1, 10, Node(0, 7, Empty, Empty), Node(0, 13, Empty, Empty)), Node(0, 20, Empty, Empty))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Left-Left rotation (RR case)`` () =
        let resultSet = Node(1, 15, Empty, Node(0, 20, Empty, Empty)) |> AVLSet.add 25

        let correctSet = Node(1, 20, Node(0, 15, Empty, Empty), Node(0, 25, Empty, Empty))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Right-Left rotation (RL case)`` () =
        let resultSet =
            Node(2, 15, Node(0, 10, Empty, Empty), Node(1, 20, Node(0, 16, Empty, Empty), Node(0, 24, Empty, Empty)))
            |> AVLSet.add 27

        let correctSet =
            Node(
                2,
                20,
                Node(1, 15, Node(0, 10, Empty, Empty), Node(0, 16, Empty, Empty)),
                Node(1, 24, Empty, Node(0, 27, Empty, Empty))
            )

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Left-Right rotation (LR case)`` () =
        let resultSet =
            Node(2, 15, Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)), Node(0, 20, Empty, Empty))
            |> AVLSet.add 14

        let correctSet =
            Node(
                2,
                12,
                Node(1, 10, Node(0, 6, Empty, Empty), Empty),
                Node(1, 15, Node(0, 14, Empty, Empty), Node(0, 20, Empty, Empty))
            )

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Single-node deletion`` () =
        let resultSet = Node(0, 15, Empty, Empty) |> AVLSet.delete 15

        let correctSet: AVLTree<int> = Empty

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Non-existent element deletion`` () =
        let resultSet = Node(1, 15, Node(0, 10, Empty, Empty), Empty) |> AVLSet.delete 20

        let correctSet = Node(1, 15, Node(0, 10, Empty, Empty), Empty)

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Leaf node deletion`` () =
        let resultSet =
            Node(1, 15, Node(0, 10, Empty, Empty), Node(0, 20, Empty, Empty))
            |> AVLSet.delete 10

        let correctSet = Node(1, 15, Empty, Node(0, 20, Empty, Empty))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Deletion with single rotation`` () =
        let resultSet =
            Node(2, 15, Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)), Node(0, 20, Empty, Empty))
            |> AVLSet.delete 20

        let correctSet =
            Node(2, 10, Node(0, 6, Empty, Empty), Node(1, 15, Node(0, 12, Empty, Empty), Empty))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Node deletion with one child`` () =
        let resultSet =
            Node(
                2,
                15,
                Node(1, 10, Empty, Node(0, 12, Empty, Empty)),
                Node(1, 20, Node(0, 16, Empty, Empty), Node(0, 24, Empty, Empty))
            )
            |> AVLSet.delete 10

        let correctSet =
            Node(2, 15, Node(0, 12, Empty, Empty), Node(1, 20, Node(0, 16, Empty, Empty), Node(0, 24, Empty, Empty)))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Root deletion with successor replacement`` () =
        let resultSet =
            Node(
                2,
                15,
                Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)),
                Node(1, 20, Node(0, 16, Empty, Empty), Node(0, 24, Empty, Empty))
            )
            |> AVLSet.delete 15

        let correctSet =
            Node(
                2,
                16,
                Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)),
                Node(1, 20, Empty, Node(0, 24, Empty, Empty))
            )

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Deletion with cascading rebalance`` () =
        let resultSet =
            Node(2, 15, Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)), Node(0, 20, Empty, Empty))
            |> AVLSet.delete 15

        let correctSet =
            Node(2, 10, Node(0, 6, Empty, Empty), Node(1, 20, Node(0, 12, Empty, Empty), Empty))

        resultSet |> should equal correctSet

    [<Fact>]
    let ``Complex multi-level deletion`` () =
        let resultSet =
            Node(
                3,
                15,
                Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)),
                Node(
                    2,
                    25,
                    Node(0, 20, Empty, Empty),
                    Node(1, 30, Node(0, 27, Empty, Empty), Node(0, 33, Empty, Empty))
                )
            )
            |> AVLSet.delete 15

        let correctSet =
            Node(
                3,
                20,
                Node(1, 10, Node(0, 6, Empty, Empty), Node(0, 12, Empty, Empty)),
                Node(2, 30, Node(1, 25, Empty, Node(0, 27, Empty, Empty)), Node(0, 33, Empty, Empty))
            )

        resultSet |> should equal correctSet

    [<Property>]
    let ``Adding elemements to set`` (elements: int list) =
        let set = elements |> List.fold (fun t x -> AVLSet.add x t) AVLSet.empty

        let rec setContainsList list set =
            match list with
            | [] -> true
            | head :: tail -> AVLSet.contains head set && setContainsList tail set

        isSetValid set None None |> should be True
        setContainsList elements set |> should be True

    [<Property>]
    let ``Set cloning`` (elements: int list) =
        let set = elements |> List.fold (fun t x -> AVLSet.add x t) AVLSet.empty

        AVLSet.copy set |> should equal set

    [<Property>]
    let ``Deleting elements from set`` (elements: int list) =
        let set = elements |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let emptySet = elements |> List.fold (fun t e -> AVLSet.delete e t) set
        let empty: AVLTree<int> = AVLSet.empty

        emptySet |> should equal empty

    [<Property>]
    let ``Standard set union`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let unionSet = AVLSet.union setA setB

        isSetValid unionSet None None |> should be True

        (advancedContains (fun v x -> x) setA unionSet
         && advancedContains (fun v x -> x) setB unionSet)
        |> should be True

        let unionSetSwapped = AVLSet.union setB setA

        (advancedContains (fun v x -> x) unionSet unionSetSwapped
         && advancedContains (fun v x -> x) unionSetSwapped unionSet)
        |> should be True


    [<Property>]
    let ``Standard set intersection`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let intersectionSet = AVLSet.intersection setA setB

        isSetValid intersectionSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then x else not x) setA intersectionSet
        |> should be True

        let intersectionSetSwapped = AVLSet.intersection setB setA

        (advancedContains (fun v x -> x) intersectionSet intersectionSetSwapped
         && advancedContains (fun v x -> x) intersectionSetSwapped intersectionSet)
        |> should be True

    [<Property>]
    let ``Standard set difference`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let differenceSet = AVLSet.difference setA setB

        isSetValid differenceSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA differenceSet
        |> should be True

    [<Property>]
    let ``Standard symmetric difference`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let symmDiffSet = AVLSet.symmDifference setA setB

        isSetValid symmDiffSet None None |> should be True

        (advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA symmDiffSet
         && advancedContains (fun v x -> if AVLSet.contains v setA then not x else x) setB symmDiffSet)
        |> should be True

        let symmDiffSetSwapped = AVLSet.symmDifference setB setA

        (advancedContains (fun v x -> x) symmDiffSet symmDiffSetSwapped
         && advancedContains (fun v x -> x) symmDiffSetSwapped symmDiffSet)
        |> should be True

    [<Property>]
    let ``Union via tree traversal`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let unionSet = AVLSet.Traversal.union setA setB

        isSetValid unionSet None None |> should be True

        (advancedContains (fun v x -> x) setA unionSet
         && advancedContains (fun v x -> x) setB unionSet)
        |> should be True

        let unionSetSwapped = AVLSet.union setB setA

        (advancedContains (fun v x -> x) unionSet unionSetSwapped
         && advancedContains (fun v x -> x) unionSetSwapped unionSet)
        |> should be True

    [<Property>]
    let ``Intersection via tree traversal`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let intersectionSet = AVLSet.Traversal.intersection setA setB

        isSetValid intersectionSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then x else not x) setA intersectionSet
        |> should be True

        let intersectionSetSwapped = AVLSet.intersection setB setA

        (advancedContains (fun v x -> x) intersectionSet intersectionSetSwapped
         && advancedContains (fun v x -> x) intersectionSetSwapped intersectionSet)
        |> should be True

    [<Property>]
    let ``Difference via tree traversal`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let differenceSet = AVLSet.Traversal.difference setA setB

        isSetValid differenceSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA differenceSet
        |> should be True

    [<Property>]
    let ``Symmetric difference via tree traversal`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let symmDiffSet = AVLSet.Traversal.symmDifference setA setB

        isSetValid symmDiffSet None None |> should be True

        (advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA symmDiffSet
         && advancedContains (fun v x -> if AVLSet.contains v setA then not x else x) setB symmDiffSet)
        |> should be True

        let symmDiffSetSwapped = AVLSet.symmDifference setB setA

        (advancedContains (fun v x -> x) symmDiffSet symmDiffSetSwapped
         && advancedContains (fun v x -> x) symmDiffSetSwapped symmDiffSet)
        |> should be True

    [<Property>]
    let ``Parallel set union with threads`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let unionSet = ParallelAVLSet.union None setA setB

        isSetValid unionSet None None |> should be True

        (advancedContains (fun v x -> x) setA unionSet
         && advancedContains (fun v x -> x) setB unionSet)
        |> should be True

        let unionSetSwapped = AVLSet.union setB setA

        (advancedContains (fun v x -> x) unionSet unionSetSwapped
         && advancedContains (fun v x -> x) unionSetSwapped unionSet)
        |> should be True

    [<Property>]
    let ``Parallel set intersection with threads`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let intersectionSet = ParallelAVLSet.intersection None setA setB

        isSetValid intersectionSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then x else not x) setA intersectionSet
        |> should be True

        let intersectionSetSwapped = AVLSet.intersection setB setA

        (advancedContains (fun v x -> x) intersectionSet intersectionSetSwapped
         && advancedContains (fun v x -> x) intersectionSetSwapped intersectionSet)
        |> should be True

    [<Property>]
    let ``Parallel set difference with threads`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let differenceSet = ParallelAVLSet.difference None setA setB

        isSetValid differenceSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA differenceSet
        |> should be True

    [<Property>]
    let ``Parallel set symmetric difference with threads`` (elementsA: int list, elementsB: int list) =
        let setA = elementsA |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let setB = elementsB |> List.fold (fun t e -> AVLSet.add e t) AVLSet.empty
        let symmDiffSet = ParallelAVLSet.symmDifference None setA setB

        isSetValid symmDiffSet None None |> should be True

        (advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA symmDiffSet
         && advancedContains (fun v x -> if AVLSet.contains v setA then not x else x) setB symmDiffSet)
        |> should be True

        let symmDiffSetSwapped = AVLSet.symmDifference setB setA

        (advancedContains (fun v x -> x) symmDiffSet symmDiffSetSwapped
         && advancedContains (fun v x -> x) symmDiffSetSwapped symmDiffSet)
        |> should be True
