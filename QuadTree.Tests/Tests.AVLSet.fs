namespace QuadTree.Tests.AVLSet

open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open QuadTree.AVLSet

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

    [<Fact>]
    let ``Search for missing element`` () =
        let set =
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

        AVLSet.contains 100 set |> should be False

    [<Fact>]
    let ``Search for negative value`` () =
        let set =
            Node(
                3,
                15,
                Node(2, 5, Node(1, 0, Node(0, -3, Empty, Empty), Node(0, 2, Empty, Empty)), Node(0, 10, Empty, Empty)),
                Node(1, 20, Node(0, 16, Empty, Empty), Node(0, 24, Empty, Empty))
            )

        AVLSet.contains -3 set |> should be True

    [<Fact>]
    let ``Tree structure cloning`` () =
        let set =
            Node(
                3,
                15,
                Node(2, 5, Node(1, 0, Node(0, -3, Empty, Empty), Node(0, 2, Empty, Empty)), Node(0, 10, Empty, Empty)),
                Node(1, 20, Node(0, 16, Empty, Empty), Node(0, 24, Empty, Empty))
            )

        AVLSet.copy set |> should equal set

    let rnd = System.Random(123561)

    let dataA = Array.init 10000 (fun _ -> rnd.Next())
    let dataB = Array.init 2000 (fun _ -> rnd.Next())

    let setA = dataA |> Array.fold (fun s v -> AVLSet.add v s) AVLSet.empty
    let setB = dataB |> Array.fold (fun s v -> AVLSet.add v s) AVLSet.empty

    [<Fact>]
    let ``100k operations stress test`` () =
        let data = Array.init 100000 (fun _ -> rnd.Next())

        let set = data |> Array.fold (fun s v -> AVLSet.add v s) AVLSet.empty

        isSetValid set None None |> should be True

    [<Fact>]
    let ``Standard set union`` () =
        let unionSet = AVLSet.union setA setB

        isSetValid unionSet None None |> should be True

        (advancedContains (fun v x -> x) setA unionSet
         && advancedContains (fun v x -> x) setB unionSet)
        |> should be True

        AVLSet.union setB setA |> should equal unionSet

    [<Fact>]
    let ``Standard set intersection`` () =
        let intersectionSet = AVLSet.intersection setA setB

        isSetValid intersectionSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then x else not x) setA intersectionSet
        |> should be True

        AVLSet.intersection setB setA |> should equal intersectionSet

    [<Fact>]
    let ``Standard set difference`` () =
        let differenceSet = AVLSet.difference setA setB

        isSetValid differenceSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA differenceSet
        |> should be True

    [<Fact>]
    let ``Standard symmetric difference`` () =
        let symmDiffSet = AVLSet.symmDifference setA setB

        isSetValid symmDiffSet None None |> should be True

        (advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA symmDiffSet
         && advancedContains (fun v x -> if AVLSet.contains v setA then not x else x) setB symmDiffSet)
        |> should be True

        AVLSet.symmDifference setB setA |> should equal symmDiffSet

    [<Fact>]
    let ``Union via tree traversal`` () =
        let unionSet = AVLSet.unionTraversal setA setB

        isSetValid unionSet None None |> should be True

        (advancedContains (fun v x -> x) setA unionSet
         && advancedContains (fun v x -> x) setB unionSet)
        |> should be True

        AVLSet.unionTraversal setB setA |> should equal unionSet

    [<Fact>]
    let ``Intersection via tree traversal`` () =
        let intersectionSet = AVLSet.intersectionTraversal setA setB

        isSetValid intersectionSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then x else not x) setA intersectionSet
        |> should be True

        AVLSet.intersectionTraversal setB setA |> should equal intersectionSet

    [<Fact>]
    let ``Difference via tree traversal`` () =
        let differenceSet = AVLSet.differenceTraversal setA setB

        isSetValid differenceSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA differenceSet
        |> should be True

    [<Fact>]
    let ``Symmetric difference via tree traversal`` () =
        let symmDiffSet = AVLSet.symmDifferenceTraversal setA setB

        isSetValid symmDiffSet None None |> should be True

        (advancedContains (fun v x -> if AVLSet.contains v setB then not x else x = true) setA symmDiffSet
         && advancedContains (fun v x -> if AVLSet.contains v setA then not x else x) setB symmDiffSet)
        |> should be True

        AVLSet.symmDifferenceTraversal setB setA |> should equal symmDiffSet

    [<Fact>]
    let ``Parallel set union with threads`` () =
        let opts = ParallelOptions()

        let unionSet = AVLSet.parallelUnion opts setA setB

        isSetValid unionSet None None |> should be True

        (advancedContains (fun v x -> x) setA unionSet
         && advancedContains (fun v x -> x) setB unionSet)
        |> should be True

        AVLSet.parallelUnion opts setB setA |> should equal unionSet

    [<Fact>]
    let ``Parallel set intersection with threads`` () =
        let opts = ParallelOptions()

        let intersectionSet = AVLSet.parallelIntersection opts setA setB

        isSetValid intersectionSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then x else not x) setA intersectionSet
        |> should be True

        AVLSet.parallelIntersection opts setB setA |> should equal intersectionSet

    [<Fact>]
    let ``Parallel set difference with threads`` () =
        let opts = ParallelOptions()

        let differenceSet = AVLSet.parallelDifference opts setA setB

        isSetValid differenceSet None None |> should be True

        advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA differenceSet
        |> should be True

    [<Fact>]
    let ``Parallel set symmetrical difference with threads`` () =
        let opts = ParallelOptions()

        let symmDiffSet = AVLSet.parallelSymmDifference opts setA setB

        isSetValid symmDiffSet None None |> should be True

        (advancedContains (fun v x -> if AVLSet.contains v setB then not x else x) setA symmDiffSet
         && advancedContains (fun v x -> if AVLSet.contains v setA then not x else x) setB symmDiffSet)
        |> should be True

        AVLSet.parallelSymmDifference opts setB setA |> should equal symmDiffSet
