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

module Error =
    let map (f: 'a -> 'b) (err: 'a) : 'b = f err

module Result =
    let mapError (f: 'e1 -> 'e2) (result: Result<'ok, 'e1>) : Result<'ok, 'e2> =
        match result with
        | Ok ok -> Ok ok
        | Error err -> Error (f err)

type ResultBuilder() =
    member _.Return(x: 'ok) : Result<'ok, 'err> = Ok x
    member _.ReturnFrom(result: Result<'ok, 'err>) : Result<'ok, 'err> = result
    member _.Bind(result: Result<'ok, 'err>, f: 'ok -> Result<'ok2, 'err>) : Result<'ok2, 'err> =
        match result with
        | Ok ok -> f ok
        | Error err -> Error err
    member _.Zero() : Result<unit, 'err> = Ok ()
    member _.Delay(f: unit -> Result<'ok, 'err>) = f
    member _.Run(f: unit -> Result<'ok, 'err>) = f ()
    member this.While(guard: unit -> bool, body: unit -> Result<unit, 'err>) =
        if guard() then
            this.Bind(body(), fun () -> this.While(guard, body))
        else
            this.Zero()
    member this.For(sequence: seq<'ok>, f: 'ok -> Result<unit, 'err>) =
        use en = sequence.GetEnumerator()
        this.While(en.MoveNext, fun () -> f en.Current)

    member _.Combine(result1: Result<unit, 'err>, result2: Result<'ok, 'err>) : Result<'ok, 'err> =
        match result1 with
        | Ok () -> result2
        | Error err -> Error err

    member this.MergeSources(result1: Result<'ok1, 'err>, result2: Result<'ok2, 'err>) : Result<'ok1 * 'ok2, 'err> =
        match result1, result2 with
        | Ok ok1, Ok ok2 -> Ok (ok1, ok2)
        | Error err, _ | _, Error err -> Error err

let result = ResultBuilder()

let powersOfTwo =
    [ 1UL
      2UL
      4UL
      8UL
      16UL
      32UL
      64UL

      128UL

      256UL
      512UL
      1024UL
      2048UL
      4096UL
      8192UL
      16384UL

      32768UL

      65536UL
      131072UL
      262144UL
      524288UL
      1048576UL
      2097152UL
      4194304UL

      8388608UL

      16777216UL
      33554432UL
      67108864UL
      134217728UL
      268435456UL
      536870912UL
      1073741824UL

      2147483648UL

      4294967296UL
      8589934592UL
      17179869184UL
      34359738368UL
      68719476736UL
      137438953472UL
      274877906944UL

      549755813888UL

      1099511627776UL
      2199023255552UL
      4398046511104UL
      8796093022208UL
      17592186044416UL
      35184372088832UL
      70368744177664UL

      140737488355328UL

      281474976710656UL
      562949953421312UL
      1125899906842624UL
      2251799813685248UL
      4503599627370496UL
      9007199254740992UL
      18014398509481984UL

      36028797018963970UL

      72057594037927940UL
      144115188075855870UL
      288230376151711740UL
      576460752303423500UL
      1152921504606847000UL
      2305843009213694000UL
      4611686018427388000UL

      9223372036854776000UL ]

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
