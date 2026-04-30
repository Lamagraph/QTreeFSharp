module RedBlackSet.Tests

open System
open RedBlackSet
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

        if lH > rH then
            if lH = -1 || rH = -1 || (float (lH + 1) / float (rH + 1) > 2) then
                -1
            else
                lH
        else if lH = -1 || rH = -1 || (float (rH + 1) / float (lH + 1) > 2) then
            -1
        else
            rH

let rec blackSonsOfRed tree =
    match tree with
    | Empty -> true
    | Node(Red, Node(Red, _, _, _), _, _) -> false
    | Node(Red, _, _, Node(Red, _, _, _)) -> false
    | Node(_, l, _, r) -> blackSonsOfRed l && blackSonsOfRed r

let rec numOfElements tree num =
    match tree with
    | Empty -> 0
    | Node(_, l, _, r) ->
        let lN = numOfElements l num
        let rN = numOfElements r num
        lN + rN + 1

[<Fact>]
let oneElement () =
    let t1 = emptySet
    let t2 = insert t1 4
    let t3 = insert t2 4
    Assert.True(contains t3 4)
    Assert.Equal(1, blHeightInv t3)
    Assert.NotEqual(-1, heightInv t3)
    Assert.True(blackSonsOfRed t3)
    Assert.Equal(1, numOfElements t3 0)

[<Fact>]
let insertSomeElem () =
    let t1 = emptySet
    let t2 = insert t1 5
    let t3 = insert t2 9
    let t4 = insert t3 -7
    let t5 = insert t4 89
    let t6 = insert t5 -27
    let t7 = insert t6 13
    Assert.True(contains t7 -7)
    Assert.Equal(2, blHeightInv t7)
    Assert.NotEqual(-1, heightInv t7)
    Assert.True(blackSonsOfRed t7)
    Assert.Equal(6, numOfElements t7 0)

[<Fact>]
let deleteSomeElem () =
    let t1 = emptySet
    let t2 = insert t1 5
    let t3 = insert t2 9
    let t4 = insert t3 -7
    let t5 = insert t4 89
    let t6 = insert t5 -27
    let t7 = insert t6 13
    let t8 = delete t7 99
    let t9 = delete t8 13
    Assert.False(contains t9 13)
    Assert.Equal(2, blHeightInv t9)
    Assert.NotEqual(-1, heightInv t9)
    Assert.True(blackSonsOfRed t9)
    Assert.Equal(5, numOfElements t9 0)

[<Fact>]
let unionSets () =
    let t1 = emptySet
    let t2 = insert t1 5
    let t3 = insert t2 9
    let t4 = insert t3 -7
    let t5 = insert t4 89
    let t6 = insert t5 -27
    let t7 = insert t6 13

    let t1' = emptySet
    let t2' = insert t1' 2
    let t3' = insert t2' 7
    let t4' = insert t3' 21
    let t5' = insert t4' 9
    let t6' = insert t5' 5

    let tU = union t7 t6'
    Assert.NotEqual(-1, heightInv tU)
    Assert.True(blackSonsOfRed tU)
    Assert.Equal(9, numOfElements tU 0)

[<Fact>]
let intersectionSets () =
    let t1 = emptySet
    let t2 = insert t1 5
    let t3 = insert t2 9
    let t4 = insert t3 -7
    let t5 = insert t4 89
    let t6 = insert t5 -27
    let t7 = insert t6 13

    let t1' = emptySet
    let t2' = insert t1' 2
    let t3' = insert t2' 7
    let t4' = insert t3' 21
    let t5' = insert t4' 9
    let t6' = insert t5' 5

    let tI = intersection t7 t6'
    Assert.NotEqual(-1, heightInv tI)
    Assert.True(blackSonsOfRed tI)
    Assert.Equal(2, numOfElements tI 0)

[<Fact>]
let differenceSets () =
    let t1 = emptySet
    let t2 = insert t1 5
    let t3 = insert t2 9
    let t4 = insert t3 -7
    let t5 = insert t4 89
    let t6 = insert t5 -27
    let t7 = insert t6 13

    let t1' = emptySet
    let t2' = insert t1' 2
    let t3' = insert t2' 7
    let t4' = insert t3' 21
    let t5' = insert t4' 9
    let t6' = insert t5' 5

    let tD = difference t7 t6'
    Assert.NotEqual(-1, heightInv tD)
    Assert.True(blackSonsOfRed tD)
    Assert.Equal(4, numOfElements tD 0)

[<Fact>]
let emptySetProperties () =
    let t = emptySet
    Assert.False(contains t 0)
    Assert.Equal(0, numOfElements t 0)
    Assert.Equal(0, blHeightInv t)
    Assert.True(blackSonsOfRed t)

[<Fact>]
let largeSetInsertion () =
    let randomValues = [ for i in 1..1000 -> Random().Next(-10000, 10000) ]
    let tree = Seq.fold (fun acc x -> insert acc x) emptySet randomValues

    Assert.NotEqual(-1, blHeightInv tree)
    Assert.True(blackSonsOfRed tree)

    for x in randomValues do
        Assert.True(contains tree x)

[<Fact>]
let deleteRoot () =
    let t1 = insert emptySet 5
    let t2 = insert t1 3
    let t3 = insert t2 7
    let t4 = delete t3 5

    Assert.False(contains t4 5)
    Assert.True(contains t4 3)
    Assert.True(contains t4 7)
    Assert.NotEqual(-1, blHeightInv t4)

[<Fact>]
let complexRedBlackViolations () =
    let values = [ 1..20 ]
    let tree = Seq.fold (fun acc x -> insert acc x) emptySet values

    Assert.NotEqual(-1, blHeightInv tree)
    Assert.True(blackSonsOfRed tree)
