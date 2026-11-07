module Common

[<Measure>]
type nvals

[<Measure>]
type storageSize

type 'value treeValue =
    | Dummy
    | UserValue of 'value

type BinSearchTree<'value> =
    | Leaf of 'value
    | Node of BinSearchTree<'value> * 'value * BinSearchTree<'value>

let treeOfPowersOfTwo =
    BinSearchTree.Node(
        BinSearchTree.Node(
            BinSearchTree.Node(
                BinSearchTree.Node(BinSearchTree.Leaf(1UL), 2UL, BinSearchTree.Leaf(4UL)),
                8UL,
                BinSearchTree.Node(BinSearchTree.Leaf(16UL), 32UL, BinSearchTree.Leaf(64UL))
            ),
            128UL,
            BinSearchTree.Node(
                BinSearchTree.Node(BinSearchTree.Leaf(256UL), 512UL, BinSearchTree.Leaf(1024UL)),
                2048UL,
                BinSearchTree.Node(BinSearchTree.Leaf(4096UL), 8192UL, BinSearchTree.Leaf(16384UL))
            )
        ),
        32768UL,
        BinSearchTree.Node(
            BinSearchTree.Node(
                BinSearchTree.Node(BinSearchTree.Leaf(65536UL), 131072UL, BinSearchTree.Leaf(262144UL)),
                524288UL,
                BinSearchTree.Node(BinSearchTree.Leaf(1048576UL), 2097152UL, BinSearchTree.Leaf(4194304UL))
            ),
            8388608UL,
            BinSearchTree.Node(
                BinSearchTree.Node(BinSearchTree.Leaf(16777216UL), 33554432UL, BinSearchTree.Leaf(67108864UL)),
                134217728UL,
                BinSearchTree.Node(BinSearchTree.Leaf(268435456UL), 536870912UL, BinSearchTree.Leaf(1073741824UL))
            )
        )
    )

let getNearestUpperPowerOfTwo (x: uint64) =
    let MAX = 9223372036854776000UL

    let rec find tree rightBound =
        match tree with
        | BinSearchTree.Node(left, v, right) ->
            if x = v then v
            elif x < v then find left v
            else find right rightBound
        | BinSearchTree.Leaf(v) -> if x <= v then v else rightBound

    if x = MAX then
        MAX
    elif x < MAX then
        find treeOfPowersOfTwo 9223372036854776000UL
    else
        failwithf "Argument is too large. Must be not greater then %A" MAX
