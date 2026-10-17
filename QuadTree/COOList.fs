module COOList

open Common
open Matrix
open COOArray

let private isStrictlySortedList (entries: COOEntry<'v> list) : bool =
    let rec loop prev rest =
        match rest with
        | [] -> true
        | (rCurr, cCurr, _) as curr :: tail ->
            let (rPrev, cPrev, _) = prev

            if compareCoords rPrev cPrev rCurr cCurr < 0 then
                loop curr tail
            else
                false

    match entries with
    | []
    | [ _ ] -> true
    | head :: tail -> loop head tail

let private areAllInBoundsList (nrows: uint64<nrows>) (ncols: uint64<ncols>) (entries: COOEntry<'v> list) : bool =
    let nr = uint64 nrows
    let nc = uint64 ncols

    let rec loop rest =
        match rest with
        | [] -> true
        | (r, c, _) :: tail -> if uint64 r < nr && uint64 c < nc then loop tail else false

    loop entries

let private sortAndDeduplicateList (entries: COOEntry<'v> list) : COOEntry<'v> list =
    if isStrictlySortedList entries then
        entries
    elif List.isEmpty entries then
        entries
    else
        let sorted = entries |> List.sortWith entryCoordCompare

        let rec deduplicate acc curEntry rest =
            match rest with
            | [] -> List.rev (curEntry :: acc)
            | ((r2, c2, _) as nextEntry) :: tail ->
                let (r1, c1, _) = curEntry

                if r1 = r2 && c1 = c2 then
                    deduplicate acc nextEntry tail
                else
                    deduplicate (curEntry :: acc) nextEntry tail

        match sorted with
        | [] -> []
        | head :: tail -> deduplicate [] head tail

[<Struct>]
type ListCOO<'value> =
    val nrows: uint64<nrows>
    val ncols: uint64<ncols>
    val entries: COOEntry<'value> list

    member this.list = this.entries

    static member private NormalizeEntries
        (nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value> list)
        : COOEntry<'value> list =
        let nr = uint64 nrows
        let nc = uint64 ncols

        let inBounds =
            if areAllInBoundsList nrows ncols entries then
                entries
            else
                entries |> List.filter (fun (r, c, _) -> uint64 r < nr && uint64 c < nc)

        sortAndDeduplicateList inBounds

    new(_nrows, _ncols, _entries: COOEntry<'value> list) =
        { nrows = _nrows
          ncols = _ncols
          entries = ListCOO<'value>.NormalizeEntries(_nrows, _ncols, _entries) }

    new(_nrows, _ncols, _entries: COOEntry<'value> list, _presorted: bool) =
        let sorted =
            if _presorted then
                _entries
            else
                ListCOO<'value>.NormalizeEntries(_nrows, _ncols, _entries)

        { nrows = _nrows
          ncols = _ncols
          entries = sorted }

    static member Create(nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value> list) : ListCOO<'value> =
        let sorted =
            if isStrictlySortedList entries && areAllInBoundsList nrows ncols entries then
                entries
            else
                ListCOO<'value>.NormalizeEntries(nrows, ncols, entries)

        ListCOO<'value>(nrows, ncols, sorted, true)

    static member Create(nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value>[]) : ListCOO<'value> =
        ListCOO<'value>.Create(nrows, ncols, Array.toList entries)

    static member TryCreate
        (nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value> list)
        : Result<ListCOO<'value>, Error> =
        if areAllInBoundsList nrows ncols entries then
            Ok(ListCOO<'value>.Create(nrows, ncols, entries))
        else
            Error Error.InconsistentSizeOfArguments

    static member TryCreate
        (nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value>[])
        : Result<ListCOO<'value>, Error> =
        ListCOO<'value>.TryCreate(nrows, ncols, Array.toList entries)

    static member internal CreatePresorted
        (nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value> list)
        : ListCOO<'value> =
#if DEBUG
        System.Diagnostics.Debug.Assert(
            isStrictlySortedList entries && areAllInBoundsList nrows ncols entries,
            "ListCOO: entries must be strictly sorted by (row, col) without duplicates and within matrix bounds."
        )
#endif
        ListCOO<'value>(nrows, ncols, entries, true)

    member this.Item
        with get (rowindex: uint64<rowindex>, colindex: uint64<colindex>): Result<option<'value>, Error> =
            match checkBounds (uint64 this.nrows) (uint64 this.ncols) (uint64 rowindex) (uint64 colindex) with
            | Error e -> Error e
            | Ok() ->
                let rec findEntry list =
                    match list with
                    | [] -> None
                    | (i, j, v) :: tail ->
                        let c = compareCoords i j rowindex colindex

                        if c = 0 then Some v
                        elif c > 0 then None
                        else findEntry tail

                Ok(findEntry this.entries)

let cooGet (coo: ListCOO<'a>, rowindex: uint64<rowindex>, colindex: uint64<colindex>) : Result<option<'a>, Error> =
    coo.[rowindex, colindex]

let fromArray (coo: ArrayCOO<'a>) : ListCOO<'a> =
    ListCOO.CreatePresorted(coo.nrows, coo.ncols, Array.toList coo.entries)

let toArray (coo: ListCOO<'a>) : ArrayCOO<'a> =
    ArrayCOO.CreatePresorted(coo.nrows, coo.ncols, Array.ofList coo.entries)

let cooUpdate
    (coo: ListCOO<'a>, rowindex: uint64<rowindex>, colindex: uint64<colindex>, value: 'a)
    : Result<ListCOO<'a>, Error> =
    match checkBounds (uint64 coo.nrows) (uint64 coo.ncols) (uint64 rowindex) (uint64 colindex) with
    | Error e -> Error e
    | Ok() ->
        let rec loop rest acc inserted =
            match rest with
            | [] ->
                if inserted then
                    List.rev acc
                else
                    List.rev ((rowindex, colindex, value) :: acc)
            | (i, j, v) :: tail ->
                if inserted then
                    loop tail ((i, j, v) :: acc) true
                else
                    let c = compareCoords rowindex colindex i j

                    if c = 0 then
                        loop tail ((rowindex, colindex, value) :: acc) true
                    elif c < 0 then
                        loop rest ((rowindex, colindex, value) :: acc) true
                    else
                        loop tail ((i, j, v) :: acc) false

        Ok(ListCOO.CreatePresorted(coo.nrows, coo.ncols, loop coo.entries [] false))

let inline private advanceCursor ri cj entries =
    match entries with
    | (ei, ej, ev) :: tail when ei = ri && ej = cj -> Some ev, tail
    | _ -> None, entries

let rec private cooMapInner (coo: ListCOO<'a>) (op: UnaryOp<'a, 'b>) : ListCOO<'b> =
    match op with
    | UnaryOp.ValuesOnly f -> cooMapInner coo (UnaryOp.ValuesOnlyIndexed(fun _ _ v -> f v))
    | UnaryOp.ValuesOnlyIndexed f ->
        let entries =
            coo.entries
            |> List.choose (fun (i, j, v) -> f i j v |> Option.map (fun r -> (i, j, r)))

        ListCOO.CreatePresorted(coo.nrows, coo.ncols, entries)
    | UnaryOp.AllCells f ->
        match f None with
        | None -> cooMapInner coo (UnaryOp.ValuesOnly(fun v -> f (Some v)))
        | Some _ -> cooMapInner coo (UnaryOp.AllCellsIndexed(fun _ _ v -> f v))
    | UnaryOp.AllCellsIndexed f ->
        let rec loopRow i curRest acc =
            if i < uint64 coo.nrows then
                let ri = i * 1UL<rowindex>

                let rec loopCol j r acc' =
                    if j < uint64 coo.ncols then
                        let cj = j * 1UL<colindex>
                        let v, nextR = advanceCursor ri cj r

                        let acc'' =
                            match f ri cj v with
                            | Some value -> (ri, cj, value) :: acc'
                            | None -> acc'

                        loopCol (j + 1UL) nextR acc''
                    else
                        r, acc'

                let nextR, nextAcc = loopCol 0UL curRest acc
                loopRow (i + 1UL) nextR nextAcc
            else
                List.rev acc

        let entries = loopRow 0UL coo.entries []
        ListCOO.CreatePresorted(coo.nrows, coo.ncols, entries)

let private mergeBinary (l1: COOEntry<'a> list) (l2: COOEntry<'b> list) (op: BinaryOp<'a, 'b, 'c>) : COOEntry<'c> list =
    let rec loop acc rest1 rest2 =
        let inline emit i j v1 v2 nextRest1 nextRest2 =
            let acc' =
                match applyBinary op i j v1 v2 with
                | Some r -> (i, j, r) :: acc
                | None -> acc

            loop acc' nextRest1 nextRest2

        match rest1, rest2 with
        | [], [] -> List.rev acc
        | (i, j, v1) :: t1, [] -> emit i j (Some v1) None t1 []
        | [], (i, j, v2) :: t2 -> emit i j None (Some v2) [] t2
        | (i1, j1, v1) :: t1, (i2, j2, v2) :: t2 ->
            let c = compareCoords i1 j1 i2 j2

            if c = 0 then emit i1 j1 (Some v1) (Some v2) t1 t2
            elif c < 0 then emit i1 j1 (Some v1) None t1 rest2
            else emit i2 j2 None (Some v2) rest1 t2

    loop [] l1 l2

let private cooMap2Inner
    (coo1: ListCOO<'a>)
    (coo2: ListCOO<'b>)
    (op: BinaryOp<'a, 'b, 'c>)
    : Result<ListCOO<'c>, Error> =
    if uint64 coo1.nrows <> uint64 coo2.nrows || uint64 coo1.ncols <> uint64 coo2.ncols then
        Error Error.InconsistentSizeOfArguments
    else
        let nrows = coo1.nrows
        let ncols = coo1.ncols
        let totalCells64 = uint64 nrows * uint64 ncols

        let isDense =
            match op with
            | BinaryOp.AllCells f -> (f None None).IsSome
            | BinaryOp.AllCellsIndexed _ -> true
            | _ -> false

        if isDense && totalCells64 > uint64 System.Int32.MaxValue then
            Error Error.InconsistentSizeOfArguments
        else
            let runDense f =
                let rec loopRow i curRest1 curRest2 acc =
                    if i < uint64 nrows then
                        let ri = i * 1UL<rowindex>

                        let rec loopCol j r1 r2 acc' =
                            if j < uint64 ncols then
                                let cj = j * 1UL<colindex>
                                let v1, nextR1 = advanceCursor ri cj r1
                                let v2, nextR2 = advanceCursor ri cj r2

                                let acc'' =
                                    match f ri cj v1 v2 with
                                    | Some value -> (ri, cj, value) :: acc'
                                    | None -> acc'

                                loopCol (j + 1UL) nextR1 nextR2 acc''
                            else
                                r1, r2, acc'

                        let nextR1, nextR2, nextAcc = loopCol 0UL curRest1 curRest2 acc
                        loopRow (i + 1UL) nextR1 nextR2 nextAcc
                    else
                        List.rev acc

                loopRow 0UL coo1.entries coo2.entries []

            let result =
                match op with
                | BinaryOp.AllCells f ->
                    match f None None with
                    | None -> mergeBinary coo1.entries coo2.entries op
                    | Some _ -> runDense (fun _ _ v1 v2 -> f v1 v2)
                | BinaryOp.AllCellsIndexed f -> runDense f
                | _ -> mergeBinary coo1.entries coo2.entries op

            ListCOO.CreatePresorted(nrows, ncols, result) |> Ok

let cooMap (coo: ListCOO<'a>) f = cooMapInner coo (UnaryOp.AllCells f)

let cooMapValues (coo: ListCOO<'a>) f = cooMapInner coo (UnaryOp.ValuesOnly f)

let cooMapi (coo: ListCOO<'a>) f =
    cooMapInner coo (UnaryOp.AllCellsIndexed f)

let cooMapiValues (coo: ListCOO<'a>) f =
    cooMapInner coo (UnaryOp.ValuesOnlyIndexed f)

let cooMap2Values (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.ValuesOnly f)

let cooMap2AllCells (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCells f)

let cooMap2 = cooMap2AllCells

let cooMap2AtLeastOne (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AtLeastOneValue f)

let cooMap2LeftValues (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.LeftValuesOnly f)

let cooMap2iValues (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.ValuesOnlyIndexed f)

let cooMap2iAllCells (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCellsIndexed f)

let cooMap2i = cooMap2iAllCells

let cooMap2iAtLeastOne (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AtLeastOneValueIndexed f)

let cooMap2iLeftValues (coo1: ListCOO<'a>) (coo2: ListCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.LeftValuesOnlyIndexed f)

let mxmcoo
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: 'a option -> 'b option -> 'c option)
    (m1: ListCOO<'a>)
    (m2: ListCOO<'b>)
    =
    if uint64 m1.ncols <> uint64 m2.nrows then
        Error Error.InconsistentSizeOfArguments
    else
        let entries1 = m1.entries
        let entries2 = m2.entries

        let checkMultCondition entries getL getR =
            let rec loop list =
                match list with
                | [] -> true
                | (_, _, v) :: tail ->
                    if Option.isNone (op_mult (getL v) (getR v)) then
                        loop tail
                    else
                        false

            loop entries

        let canOptimize =
            Option.isNone (op_mult None None)
            && checkMultCondition entries1 (fun v -> Some v) (fun _ -> None)
            && checkMultCondition entries2 (fun _ -> None) (fun v -> Some v)

        let generalResult () =
            let m1Map = entries1 |> List.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofList
            let m2Map = entries2 |> List.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofList
            let kCount = uint64 m1.ncols

            let rec loopRow i acc =
                if i < uint64 m1.nrows then
                    let ri = i * 1UL<rowindex>

                    let rec loopCol j colAcc =
                        if j < uint64 m2.ncols then
                            let cj = j * 1UL<colindex>

                            let rec loopK k kAcc =
                                if k < kCount then
                                    let a = m1Map |> Map.tryFind (ri, k * 1UL<colindex>)
                                    let b = m2Map |> Map.tryFind (k * 1UL<rowindex>, cj)
                                    loopK (k + 1UL) (op_add kAcc (op_mult a b))
                                else
                                    kAcc

                            let sum = loopK 0UL None

                            let colAcc' =
                                match sum with
                                | Some value -> (ri, cj, value) :: colAcc
                                | None -> colAcc

                            loopCol (j + 1UL) colAcc'
                        else
                            colAcc

                    let rowAcc = loopCol 0UL acc
                    loopRow (i + 1UL) rowAcc
                else
                    List.rev acc

            let result = loopRow 0UL []
            ListCOO.CreatePresorted(m1.nrows, m2.ncols, result)

        if canOptimize then
            let m2ByRow = entries2 |> List.groupBy (fun (k, _, _) -> k) |> Map.ofList

            let rec emitProducts m1List acc =
                match m1List with
                | [] -> acc
                | (i, k, v1) :: tail ->
                    let kAsRow = uint64 k * 1UL<rowindex>

                    let acc' =
                        match m2ByRow |> Map.tryFind kAsRow with
                        | Some m2Entries ->
                            let rec loopM2 m2List curAcc =
                                match m2List with
                                | [] -> curAcc
                                | (_, j, v2) :: m2Tail ->
                                    match op_mult (Some v1) (Some v2) with
                                    | Some product -> loopM2 m2Tail ((i, j, product) :: curAcc)
                                    | None -> loopM2 m2Tail curAcc

                            loopM2 m2Entries acc
                        | None -> acc

                    emitProducts tail acc'

            let result = emitProducts entries1 []

            let canMerge =
                canMergeOpAdd op_add
                && List.forall (fun (_, _, v) -> isAddNeutral op_add v) result

            if canMerge then
                let sorted = result |> List.sortWith entryCoordCompare

                let rec reduceAdjacent acc curR curC curSum rest =
                    match rest with
                    | [] ->
                        match curSum with
                        | Some v -> List.rev ((curR, curC, v) :: acc)
                        | None -> List.rev acc
                    | (i, j, v) :: tail ->
                        if i = curR && j = curC then
                            reduceAdjacent acc curR curC (op_add curSum (Some v)) tail
                        else
                            let acc' =
                                match curSum with
                                | Some sv -> (curR, curC, sv) :: acc
                                | None -> acc

                            reduceAdjacent acc' i j (Some v) tail

                let grouped =
                    match sorted with
                    | [] -> []
                    | (i, j, v) :: tail -> reduceAdjacent [] i j (Some v) tail

                ListCOO.CreatePresorted(m1.nrows, m2.ncols, grouped) |> Ok
            else
                generalResult () |> Ok
        else
            generalResult () |> Ok
