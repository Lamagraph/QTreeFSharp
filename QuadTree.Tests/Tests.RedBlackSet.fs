module RedBlackSet.Tests

open System
open QuadTree.RBSet.Tree
open QuadTree.RBSet.RBSet
open QuadTree.RBSet
open Xunit

let rec blHeightInv tree =
    match tree with
    | Empty -> 0
    | Node(color, l, _, r) ->
        let lH = blHeightInv l
        let rH = blHeightInv r

        if lH = -1 || rH = -1 || lH <> rH then -1
        else if color = Red then lH
        else lH + 1

let rec heightInv tree =
    match tree with
    | Empty -> 0
    | Node(_, l, _, r) ->
        let lH = heightInv l
        let rH = heightInv r

        if lH = -1 || rH = -1 || (float (rH + 1) / float (lH + 1) > 2) then
            -1
        else if lH > rH then
            lH + 1
        else
            rH + 1

let rec blackChildrenOfRed tree =
    match tree with
    | Empty -> true
    | Node(Red, Node(Red, _, _, _), _, _)
    | Node(Red, _, _, Node(Red, _, _, _)) -> false
    | Node(_, l, _, r) -> blackChildrenOfRed l && blackChildrenOfRed r

let rec numOfElements tree num =
    match tree with
    | Empty -> 0
    | Node(_, l, _, r) ->
        let lN = numOfElements l num
        let rN = numOfElements r num
        lN + rN + 1

[<Fact>]
let oneElement () =
    let finalTree = empty |> add 4 |> Result.bind (add 4)

    match finalTree with
    | Ok t ->
        Assert.True(contains 4 t)
        Assert.Equal(1, blHeightInv t)
        Assert.NotEqual(-1, heightInv t)
        Assert.True(blackChildrenOfRed t)
        Assert.Equal(1, numOfElements t 0)
    | Error e -> Assert.Fail $"Expect Ok, but get Error: {e}"

[<Fact>]
let insertSomeElem () =
    let finalTree =
        empty
        |> add 5
        |> Result.bind (add 9)
        |> Result.bind (add -7)
        |> Result.bind (add 89)
        |> Result.bind (add -27)
        |> Result.bind (add 13)

    match finalTree with
    | Ok t ->
        Assert.True(contains -7 t)
        Assert.Equal(2, blHeightInv t)
        Assert.NotEqual(-1, heightInv t)
        Assert.True(blackChildrenOfRed t)
        Assert.Equal(6, numOfElements t 0)
    | Error e -> Assert.Fail $"Expect Ok, but get Error: {e}"

[<Fact>]
let deleteSomeElem () =
    let finalTree =
        empty
        |> add 5
        |> Result.bind (add 9)
        |> Result.bind (add -7)
        |> Result.bind (add 89)
        |> Result.bind (add -27)
        |> Result.bind (add 13)
        |> Result.bind (delete 99)
        |> Result.bind (delete 13)

    match finalTree with
    | Ok t ->
        Assert.False(contains 13 t)
        Assert.Equal(2, blHeightInv t)
        Assert.NotEqual(-1, heightInv t)
        Assert.True(blackChildrenOfRed t)
        Assert.Equal(5, numOfElements t 0)
    | Error e -> Assert.Fail $"Expect Ok, but get Error: {e}"

[<Fact>]
let unionSets () =
    let finalTree1 =
        empty
        |> add 5
        |> Result.bind (add 9)
        |> Result.bind (add -7)
        |> Result.bind (add 89)
        |> Result.bind (add -27)
        |> Result.bind (add 13)

    let finalTree2 =
        empty
        |> add 2
        |> Result.bind (add 7)
        |> Result.bind (add 21)
        |> Result.bind (add 9)
        |> Result.bind (add 5)

    match finalTree1, finalTree2 with
    | Ok t1, Ok t2 ->
        match union t1 t2 with
        | Ok tU ->
            Assert.NotEqual(-1, heightInv tU)
            Assert.True(blackChildrenOfRed tU)
            Assert.Equal(9, numOfElements tU 0)
        | Error e -> Assert.Fail $"Error in union: {e}"
    | _ -> Assert.Fail $"Error in insert"

[<Fact>]
let intersectionSets () =
    let finalTree1 =
        empty
        |> add 5
        |> Result.bind (add 9)
        |> Result.bind (add -7)
        |> Result.bind (add 89)
        |> Result.bind (add -27)
        |> Result.bind (add 13)

    let finalTree2 =
        empty
        |> add 2
        |> Result.bind (add 7)
        |> Result.bind (add 21)
        |> Result.bind (add 9)
        |> Result.bind (add 5)

    match finalTree1, finalTree2 with
    | Ok t1, Ok t2 ->
        match intersection t1 t2 with
        | Ok tI ->
            Assert.NotEqual(-1, heightInv tI)
            Assert.True(blackChildrenOfRed tI)
            Assert.Equal(2, numOfElements tI 0)
        | Error e -> Assert.Fail $"Error in intersection: {e}"
    | _ -> Assert.Fail $"Error in insert"

[<Fact>]
let differenceSets () =
    let finalTree1 =
        empty
        |> add 5
        |> Result.bind (add 9)
        |> Result.bind (add -7)
        |> Result.bind (add 89)
        |> Result.bind (add -27)
        |> Result.bind (add 13)

    let finalTree2 =
        empty
        |> add 2
        |> Result.bind (add 7)
        |> Result.bind (add 21)
        |> Result.bind (add 9)
        |> Result.bind (add 5)

    match finalTree1, finalTree2 with
    | Ok t1, Ok t2 ->
        match difference t1 t2 with
        | Ok tD ->
            Assert.NotEqual(-1, heightInv tD)
            Assert.True(blackChildrenOfRed tD)
            Assert.Equal(4, numOfElements tD 0)
        | Error e -> Assert.Fail $"Error in difference: {e}"
    | _ -> Assert.Fail $"Error in insert"

[<Fact>]
let emptySetProperties () =
    let t = empty
    Assert.False(contains 0 t)
    Assert.Equal(0, numOfElements t 0)
    Assert.Equal(0, blHeightInv t)
    Assert.True(blackChildrenOfRed t)

[<Fact>]
let emptyResultOfIntersection () =
    let finalTree1 = empty |> add 4 |> Result.bind (add 7) |> Result.bind (add 14)

    let finalTree2 = empty |> add 8 |> Result.bind (add 10) |> Result.bind (add 13)

    match finalTree1, finalTree2 with
    | Ok t1, Ok t2 ->
        match intersection t1 t2 with
        | Ok empty -> Assert.True(true)
        | _ -> Assert.Fail $"expected Ok empty"
    | _ -> Assert.Fail $"expexcted Ok"

    match finalTree1 with
    | Ok t ->
        match intersection empty t with
        | Ok empty -> Assert.True(true)
        | _ -> Assert.Fail $"expected Ok empty"
    | _ -> Assert.Fail $"expexcted Ok"

[<Fact>]
let emptyResultOfDifference () =
    let finalTree1 = empty |> add 4 |> Result.bind (add 7) |> Result.bind (add 14)

    match finalTree1 with
    | Ok t ->
        match difference empty t with
        | Ok empty -> Assert.True(true)
        | _ -> Assert.Fail $"expected Ok empty"
    | _ -> Assert.Fail $"expexcted Ok"

[<Fact>]
let largeSetInsertion () =
    let rng = Random()
    let randomValues = [ for _ in 1..1000 -> rng.Next(-5000, 5000) ]

    let treeResult =
        randomValues |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)

    match treeResult with
    | Ok tree ->
        Assert.NotEqual(-1, blHeightInv tree)
        Assert.True(blackChildrenOfRed tree)

        for x in randomValues do
            Assert.True(contains x tree)
    | Error e -> Assert.Fail $"Error in insert: {e}"

[<Fact>]
let deleteRoot () =
    let finalTree =
        empty
        |> add 5
        |> Result.bind (add 3)
        |> Result.bind (add 7)
        |> Result.bind (delete 5)

    match finalTree with
    | Ok t ->
        Assert.False(contains 5 t)
        Assert.True(contains 3 t)
        Assert.True(contains 7 t)
        Assert.NotEqual(-1, blHeightInv t)
    | _ -> Assert.Fail $"Expect Ok, but get Error"

[<Fact>]
let complexRedBlackViolations () =
    let values = [ 1..20 ]

    let treeResult =
        values |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)

    match treeResult with
    | Ok tree ->
        Assert.NotEqual(-1, blHeightInv tree)
        Assert.True(blackChildrenOfRed tree)
    | Error e -> Assert.Fail $"Error in insert: {e}"

[<Fact>]
let randomDeletions () =
    let rng = Random()
    let insertValues = [ for _ in 1..500 -> rng.Next(-5000, 5000) ]
    let uniqueInserts = insertValues |> List.distinct

    let treeResult =
        insertValues |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)

    match treeResult with
    | Ok tree ->
        let deleteValues = uniqueInserts |> List.filter (fun _ -> rng.Next(0, 2) = 0)

        let remaining = uniqueInserts |> List.except deleteValues

        let afterDelete =
            deleteValues |> List.fold (fun acc x -> acc |> Result.bind (delete x)) (Ok tree)

        match afterDelete with
        | Ok t ->
            Assert.NotEqual(-1, blHeightInv t)
            Assert.NotEqual(-1, heightInv t)
            Assert.True(blackChildrenOfRed t)

            for x in deleteValues do
                Assert.False(contains x t, sprintf "Element %d should be deleted" x)

            for x in remaining do
                Assert.True(contains x t, sprintf "Element %d should be present" x)

            Assert.Equal(remaining.Length, numOfElements t 0)
        | Error e -> Assert.Fail $"Error in delete: {e}"
    | Error e -> Assert.Fail $"Error in insert: {e}"

[<Fact>]
let randomDeletionsOfMissingElements () =
    let rng = Random()
    let insertValues = [ for _ in 1..300 -> rng.Next(-3000, 3000) ]
    let deleteMissing = [ for _ in 1..300 -> rng.Next(10000, 20000) ]

    let treeResult =
        insertValues |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)

    match treeResult with
    | Ok tree ->
        let afterDelete =
            deleteMissing
            |> List.fold (fun acc x -> acc |> Result.bind (delete x)) (Ok tree)

        match afterDelete with
        | Ok t ->
            Assert.NotEqual(-1, blHeightInv t)
            Assert.NotEqual(-1, heightInv t)
            Assert.True(blackChildrenOfRed t)

            let expected = insertValues |> List.distinct

            for x in expected do
                Assert.True(contains x t, sprintf "Element %d should still be present" x)

            for x in deleteMissing do
                Assert.False(contains x t, sprintf "Element %d should not be present" x)

            Assert.Equal(expected.Length, numOfElements t 0)
        | Error e -> Assert.Fail $"Error in delete: {e}"
    | Error e -> Assert.Fail $"Error in insert: {e}"

[<Fact>]
let randomUnion () =
    let rng = Random()
    let vals1 = [ for _ in 1..300 -> rng.Next(-3000, 3000) ]
    let vals2 = [ for _ in 1..300 -> rng.Next(-3000, 3000) ]

    let t1Result =
        vals1 |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)

    let t2Result =
        vals2 |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)

    match t1Result, t2Result with
    | Ok t1, Ok t2 ->
        match union t1 t2 with
        | Ok tU ->
            Assert.NotEqual(-1, blHeightInv tU)
            Assert.NotEqual(-1, heightInv tU)
            Assert.True(blackChildrenOfRed tU)

            let expected = (vals1 @ vals2) |> List.distinct

            for x in expected do
                Assert.True(contains x tU, sprintf "Element %d should be in union" x)

            Assert.Equal(expected.Length, numOfElements tU 0)
        | Error e -> Assert.Fail $"Error in union: {e}"
    | _ -> Assert.Fail $"Error in insert"

[<Fact>]
let randomIntersection () =
    let rng = Random()
    let vals1 = [ for _ in 1..300 -> rng.Next(-3000, 3000) ]
    let vals2 = [ for _ in 1..300 -> rng.Next(-3000, 3000) ]

    let t1Result =
        vals1 |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)

    let t2Result =
        vals2 |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)

    match t1Result, t2Result with
    | Ok t1, Ok t2 ->
        match intersection t1 t2 with
        | Ok tI ->
            Assert.NotEqual(-1, blHeightInv tI)
            Assert.NotEqual(-1, heightInv tI)
            Assert.True(blackChildrenOfRed tI)

            let set1 = vals1 |> Set.ofList
            let set2 = vals2 |> Set.ofList
            let expected = Set.intersect set1 set2
            let allVals = Set.union set1 set2
            let notExpected = Set.difference allVals expected

            for x in expected do
                Assert.True(contains x tI, sprintf "Element %d should be in intersection" x)

            for x in notExpected do
                Assert.False(contains x tI, sprintf "Element %d should not be in intersection" x)

            Assert.Equal(expected.Count, numOfElements tI 0)
        | Error e -> Assert.Fail $"Error in intersection: {e}"
    | _ -> Assert.Fail $"Error in insert"

[<Fact>]
let randomDifference () =
    let rng = Random()
    let vals1 = [ for _ in 1..300 -> rng.Next(-3000, 3000) ]
    let vals2 = [ for _ in 1..300 -> rng.Next(-3000, 3000) ]

    let t1Result =
        vals1 |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)

    let t2Result =
        vals2 |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)

    match t1Result, t2Result with
    | Ok t1, Ok t2 ->
        match difference t1 t2 with
        | Ok tD ->
            Assert.NotEqual(-1, blHeightInv tD)
            Assert.NotEqual(-1, heightInv tD)
            Assert.True(blackChildrenOfRed tD)

            let set1 = vals1 |> Set.ofList
            let set2 = vals2 |> Set.ofList
            let expected = Set.difference set1 set2

            for x in expected do
                Assert.True(contains x tD, sprintf "Element %d should be in difference" x)

            for x in set2 do
                Assert.False(contains x tD, sprintf "Element %d should not be in difference" x)

            Assert.Equal(expected.Count, numOfElements tD 0)
        | Error e -> Assert.Fail $"Error in difference: {e}"
    | _ -> Assert.Fail $"Error in insert"

[<Fact>]
let randomMixedOperations () =
    let rng = Random()

    let buildRandomSet size =
        let values = [ for _ in 1..size -> rng.Next(-5000, 5000) ]

        values
        |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)
        |> function
            | Ok t -> t
            | Error e -> failwithf "Insert failed: %A" e

    let t1 = buildRandomSet 400
    let t2 = buildRandomSet 400

    let combined =
        match union t1 t2 with
        | Ok u -> u
        | Error e -> failwithf "Union failed: %A" e

    let combinedList =
        let rec toList tree acc =
            match tree with
            | Empty -> acc
            | Node(_, l, v, r) -> toList l (v :: toList r acc)

        toList combined []

    let toDelete = combinedList |> List.filter (fun _ -> rng.Next(0, 2) = 0)

    let afterDelete =
        toDelete |> List.fold (fun acc x -> acc |> Result.bind (delete x)) (Ok combined)

    match afterDelete with
    | Ok final ->
        Assert.NotEqual(-1, blHeightInv final)
        Assert.NotEqual(-1, heightInv final)
        Assert.True(blackChildrenOfRed t1)
        Assert.True(blackChildrenOfRed t2)
        Assert.True(blackChildrenOfRed final)

        for x in toDelete do
            Assert.False(contains x final, sprintf "Deleted element %d found" x)

        let expectedRemaining = combinedList |> List.except toDelete

        for x in expectedRemaining do
            Assert.True(contains x final, sprintf "Element %d should be present" x)

        Assert.Equal(expectedRemaining.Length, numOfElements final 0)
    | Error e -> Assert.Fail $"Error in mixed ops: {e}"
