module COOArray

open Common
open Matrix

let inline private truncate (total: int) (arr: 'T[]) : 'T[] =
    if total = arr.Length then arr else Array.sub arr 0 total

let inline private advanceArrayCursor
    (ri: uint64<rowindex>)
    (cj: uint64<colindex>)
    (arr: COOEntry<'v>[])
    (ptr: int)
    : Option<'v> * int =
    if ptr < arr.Length then
        let (ei, ej, ev) = arr.[ptr]
        if ei = ri && ej = cj then Some ev, ptr + 1 else None, ptr
    else
        None, ptr

let inline private binarySearchEntries
    (arr: COOEntry<'v>[])
    (targetR: uint64<rowindex>)
    (targetC: uint64<colindex>)
    : int =
    let rec loop low high =
        if low <= high then
            let mid = low + ((high - low) >>> 1)
            let (r, c, _) = arr.[mid]
            let cmp = compareCoords targetR targetC r c

            if cmp = 0 then mid
            elif cmp < 0 then loop low (mid - 1)
            else loop (mid + 1) high
        else
            ~~~low

    loop 0 (arr.Length - 1)

let private isStrictlySorted (arr: COOEntry<'v>[]) =
    let rec loop i =
        if i >= arr.Length then
            true
        else
            let (rPrev, cPrev, _) = arr.[i - 1]
            let (rCurr, cCurr, _) = arr.[i]

            if compareCoords rPrev cPrev rCurr cCurr < 0 then
                loop (i + 1)
            else
                false

    arr.Length <= 1 || loop 1

let private areAllInBounds nrows ncols (arr: COOEntry<'v>[]) =
    let rec loop i =
        if i >= arr.Length then
            true
        else
            let (r, c, _) = arr.[i]

            if uint64 r < uint64 nrows && uint64 c < uint64 ncols then
                loop (i + 1)
            else
                false

    loop 0

let private sortAndDeduplicate (arr: COOEntry<'v>[]) : COOEntry<'v>[] =
    if isStrictlySorted arr then
        arr
    elif arr.Length = 0 then
        arr
    else
        let tagged =
            Array.init arr.Length (fun idx ->
                let (r, c, v) = arr.[idx]
                (r, c, v, idx))

        Array.sortInPlaceWith
            (fun (r1, c1, _, idx1) (r2, c2, _, idx2) ->
                let c = compareCoords r1 c1 r2 c2
                if c <> 0 then c else compare idx1 idx2)
            tagged

        let rec countUnique i count =
            if i < tagged.Length then
                let (r1, c1, _, _) = tagged.[i]

                let rec skipDups j =
                    if j < tagged.Length then
                        let (r2, c2, _, _) = tagged.[j]
                        if r1 = r2 && c1 = c2 then skipDups (j + 1) else j
                    else
                        j

                countUnique (skipDups (i + 1)) (count + 1)
            else
                count

        let total = countUnique 0 0
        let result = Array.zeroCreate total

        let rec emitUnique i outIdx =
            if i < tagged.Length then
                let (r1, c1, _, _) = tagged.[i]

                let rec findLast j lastIdx =
                    if j < tagged.Length then
                        let (r2, c2, _, _) = tagged.[j]

                        if r1 = r2 && c1 = c2 then
                            findLast (j + 1) j
                        else
                            j, lastIdx
                    else
                        j, lastIdx

                let nextI, lastDupIdx = findLast (i + 1) i
                let (r, c, v, _) = tagged.[lastDupIdx]
                result.[outIdx] <- (r, c, v)
                emitUnique nextI (outIdx + 1)
            else
                ()

        emitUnique 0 0
        result

[<Struct>]
type ArrayCOO<'value> =
    val nrows: uint64<nrows>
    val ncols: uint64<ncols>
    val entries: COOEntry<'value>[]

    member this.list = this.entries

    static member private NormalizeEntries
        (nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value>[])
        : COOEntry<'value>[] =
        let inBounds =
            if areAllInBounds nrows ncols entries then
                entries
            else
                entries
                |> Array.filter (fun (r, c, _) -> uint64 r < uint64 nrows && uint64 c < uint64 ncols)

        sortAndDeduplicate inBounds

    new(_nrows, _ncols, _entries: COOEntry<'value>[]) =
        { nrows = _nrows
          ncols = _ncols
          entries = ArrayCOO<'value>.NormalizeEntries(_nrows, _ncols, _entries) }

    new(_nrows, _ncols, _list: COOEntry<'value> list) = ArrayCOO<'value>(_nrows, _ncols, List.toArray _list)

    new(_nrows, _ncols, _entries: COOEntry<'value>[], _presorted: bool) =
        let sorted =
            if _presorted then
                _entries
            else
                ArrayCOO<'value>.NormalizeEntries(_nrows, _ncols, _entries)

        { nrows = _nrows
          ncols = _ncols
          entries = sorted }

    static member Create(nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value>[]) : ArrayCOO<'value> =
        let sorted =
            if isStrictlySorted entries && areAllInBounds nrows ncols entries then
                entries
            else
                ArrayCOO<'value>.NormalizeEntries(nrows, ncols, entries)

        ArrayCOO<'value>(nrows, ncols, sorted, true)

    static member Create
        (nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value> list)
        : ArrayCOO<'value> =
        ArrayCOO<'value>.Create(nrows, ncols, List.toArray entries)

    static member TryCreate
        (nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value>[])
        : Result<ArrayCOO<'value>, Error> =
        if areAllInBounds nrows ncols entries then
            Ok(ArrayCOO<'value>.Create(nrows, ncols, entries))
        else
            Error Error.InconsistentSizeOfArguments

    static member TryCreate
        (nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value> list)
        : Result<ArrayCOO<'value>, Error> =
        ArrayCOO<'value>.TryCreate(nrows, ncols, List.toArray entries)

    static member internal CreatePresorted
        (nrows: uint64<nrows>, ncols: uint64<ncols>, entries: COOEntry<'value>[])
        : ArrayCOO<'value> =
#if DEBUG
        System.Diagnostics.Debug.Assert(
            isStrictlySorted entries && areAllInBounds nrows ncols entries,
            "ArrayCOO: entries must be strictly sorted by (row, col) without duplicates and within matrix bounds."
        )
#endif
        ArrayCOO<'value>(nrows, ncols, entries, true)

    member this.Item
        with get (rowindex: uint64<rowindex>, colindex: uint64<colindex>): Result<option<'value>, Error> =
            match checkBounds (uint64 this.nrows) (uint64 this.ncols) (uint64 rowindex) (uint64 colindex) with
            | Error e -> Error e
            | Ok() ->
                let idx = binarySearchEntries this.entries rowindex colindex

                if idx >= 0 then
                    let (_, _, value) = this.entries.[idx]
                    Ok(Some value)
                else
                    Ok None

let cooGet (coo: ArrayCOO<'a>, rowindex: uint64<rowindex>, colindex: uint64<colindex>) : Result<option<'a>, Error> =
    coo.[rowindex, colindex]

let cooUpdate
    (coo: ArrayCOO<'a>, rowindex: uint64<rowindex>, colindex: uint64<colindex>, value: 'a)
    : Result<ArrayCOO<'a>, Error> =
    match checkBounds (uint64 coo.nrows) (uint64 coo.ncols) (uint64 rowindex) (uint64 colindex) with
    | Error e -> Error e
    | Ok() ->
        let idx = binarySearchEntries coo.entries rowindex colindex

        if idx >= 0 then
            let arr = Array.copy coo.entries
            arr.[idx] <- (rowindex, colindex, value)
            Ok(ArrayCOO.CreatePresorted(coo.nrows, coo.ncols, arr))
        else
            let insertAt = ~~~idx
            let arr = Array.zeroCreate (coo.entries.Length + 1)
            Array.blit coo.entries 0 arr 0 insertAt
            arr.[insertAt] <- (rowindex, colindex, value)
            Array.blit coo.entries insertAt arr (insertAt + 1) (coo.entries.Length - insertAt)

            Ok(ArrayCOO.CreatePresorted(coo.nrows, coo.ncols, arr))

let rec private cooMapInner (coo: ArrayCOO<'a>) (op: UnaryOp<'a, 'b>) : ArrayCOO<'b> =
    match op with
    | UnaryOp.ValuesOnly f -> cooMapInner coo (UnaryOp.ValuesOnlyIndexed(fun _ _ v -> f v))
    | UnaryOp.ValuesOnlyIndexed f ->
        let target = Array.zeroCreate coo.entries.Length

        let rec loop inIdx outIdx =
            if inIdx < coo.entries.Length then
                let (i, j, v) = coo.entries.[inIdx]

                match f i j v with
                | Some r ->
                    target.[outIdx] <- (i, j, r)
                    loop (inIdx + 1) (outIdx + 1)
                | None -> loop (inIdx + 1) outIdx
            else
                outIdx

        let total = loop 0 0
        let res = truncate total target
        ArrayCOO.CreatePresorted(coo.nrows, coo.ncols, res)
    | UnaryOp.AllCells f ->
        match f None with
        | None -> cooMapInner coo (UnaryOp.ValuesOnly(fun v -> f (Some v)))
        | Some _ -> cooMapInner coo (UnaryOp.AllCellsIndexed(fun _ _ v -> f v))
    | UnaryOp.AllCellsIndexed f ->
        let totalCells64 = uint64 coo.nrows * uint64 coo.ncols

        if totalCells64 > uint64 System.Int32.MaxValue then
            let rec loopRow i ptr acc =
                if i < uint64 coo.nrows then
                    let ri = i * 1UL<rowindex>

                    let rec loopCol j curPtr curAcc =
                        if j < uint64 coo.ncols then
                            let cj = j * 1UL<colindex>
                            let v, nextPtr = advanceArrayCursor ri cj coo.entries curPtr

                            match f ri cj v with
                            | Some r -> loopCol (j + 1UL) nextPtr ((ri, cj, r) :: curAcc)
                            | None -> loopCol (j + 1UL) nextPtr curAcc
                        else
                            curPtr, curAcc

                    let nextPtr, nextAcc = loopCol 0UL ptr acc
                    loopRow (i + 1UL) nextPtr nextAcc
                else
                    List.rev acc

            let entries = loopRow 0UL 0 [] |> List.toArray
            ArrayCOO.CreatePresorted(coo.nrows, coo.ncols, entries)
        else
            let totalCells = int totalCells64
            let target = Array.zeroCreate totalCells

            let rec loopRow i ptr outIdx =
                if i < uint64 coo.nrows then
                    let ri = i * 1UL<rowindex>

                    let rec loopCol j curPtr curOut =
                        if j < uint64 coo.ncols then
                            let cj = j * 1UL<colindex>
                            let v, nextPtr = advanceArrayCursor ri cj coo.entries curPtr

                            match f ri cj v with
                            | Some r ->
                                target.[curOut] <- (ri, cj, r)
                                loopCol (j + 1UL) nextPtr (curOut + 1)
                            | None -> loopCol (j + 1UL) nextPtr curOut
                        else
                            curPtr, curOut

                    let nextPtr, nextOut = loopCol 0UL ptr outIdx
                    loopRow (i + 1UL) nextPtr nextOut
                else
                    outIdx

            let total = loopRow 0UL 0 0
            let finalArr = truncate total target
            ArrayCOO.CreatePresorted(coo.nrows, coo.ncols, finalArr)

let private mergeBinary (a1: COOEntry<'a>[]) (a2: COOEntry<'b>[]) (op: BinaryOp<'a, 'b, 'c>) : COOEntry<'c>[] =
    let target = Array.zeroCreate (a1.Length + a2.Length)

    let rec loop p1 p2 outIdx =
        let inline emit i j v1 v2 nextP1 nextP2 =
            match applyBinary op i j v1 v2 with
            | Some r ->
                target.[outIdx] <- (i, j, r)
                loop nextP1 nextP2 (outIdx + 1)
            | None -> loop nextP1 nextP2 outIdx

        if p1 >= a1.Length && p2 >= a2.Length then
            outIdx
        elif p1 >= a1.Length then
            let (i, j, v2) = a2.[p2]
            emit i j None (Some v2) p1 (p2 + 1)
        elif p2 >= a2.Length then
            let (i, j, v1) = a1.[p1]
            emit i j (Some v1) None (p1 + 1) p2
        else
            let (i1, j1, v1) = a1.[p1]
            let (i2, j2, v2) = a2.[p2]
            let c = compareCoords i1 j1 i2 j2

            if c = 0 then
                emit i1 j1 (Some v1) (Some v2) (p1 + 1) (p2 + 1)
            elif c < 0 then
                emit i1 j1 (Some v1) None (p1 + 1) p2
            else
                emit i2 j2 None (Some v2) p1 (p2 + 1)

    let total = loop 0 0 0
    truncate total target

let private cooMap2Inner
    (coo1: ArrayCOO<'a>)
    (coo2: ArrayCOO<'b>)
    (op: BinaryOp<'a, 'b, 'c>)
    : Result<ArrayCOO<'c>, Error> =
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
            let totalCells = if isDense then int totalCells64 else 0

            let runDense f =
                let target = Array.zeroCreate totalCells

                let rec loopRow i p1 p2 outIdx =
                    if i < uint64 nrows then
                        let ri = i * 1UL<rowindex>

                        let rec loopCol j curP1 curP2 curOut =
                            if j < uint64 ncols then
                                let cj = j * 1UL<colindex>
                                let v1, nextP1 = advanceArrayCursor ri cj coo1.entries curP1
                                let v2, nextP2 = advanceArrayCursor ri cj coo2.entries curP2

                                match f ri cj v1 v2 with
                                | Some r ->
                                    target.[curOut] <- (ri, cj, r)
                                    loopCol (j + 1UL) nextP1 nextP2 (curOut + 1)
                                | None -> loopCol (j + 1UL) nextP1 nextP2 curOut
                            else
                                curP1, curP2, curOut

                        let nextP1, nextP2, nextOut = loopCol 0UL p1 p2 outIdx
                        loopRow (i + 1UL) nextP1 nextP2 nextOut
                    else
                        outIdx

                let total = loopRow 0UL 0 0 0
                truncate total target

            let result =
                match op with
                | BinaryOp.AllCells f ->
                    match f None None with
                    | None -> mergeBinary coo1.entries coo2.entries op
                    | Some _ -> runDense (fun _ _ v1 v2 -> f v1 v2)
                | BinaryOp.AllCellsIndexed f -> runDense f
                | _ -> mergeBinary coo1.entries coo2.entries op

            ArrayCOO.CreatePresorted(nrows, ncols, result) |> Ok

let cooMap (coo: ArrayCOO<'a>) f = cooMapInner coo (UnaryOp.AllCells f)

let cooMapValues (coo: ArrayCOO<'a>) f = cooMapInner coo (UnaryOp.ValuesOnly f)

let cooMapi (coo: ArrayCOO<'a>) f =
    cooMapInner coo (UnaryOp.AllCellsIndexed f)

let cooMapiValues (coo: ArrayCOO<'a>) f =
    cooMapInner coo (UnaryOp.ValuesOnlyIndexed f)

let cooMap2Values (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.ValuesOnly f)

let cooMap2AllCells (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCells f)

let cooMap2 = cooMap2AllCells

let cooMap2AtLeastOne (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AtLeastOneValue f)

let cooMap2LeftValues (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.LeftValuesOnly f)

let cooMap2iValues (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.ValuesOnlyIndexed f)

let cooMap2iAllCells (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AllCellsIndexed f)

let cooMap2i = cooMap2iAllCells

let cooMap2iAtLeastOne (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.AtLeastOneValueIndexed f)

let cooMap2iLeftValues (coo1: ArrayCOO<'a>) (coo2: ArrayCOO<'b>) f =
    cooMap2Inner coo1 coo2 (BinaryOp.LeftValuesOnlyIndexed f)

let mxmcoo
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: 'a option -> 'b option -> 'c option)
    (m1: ArrayCOO<'a>)
    (m2: ArrayCOO<'b>)
    : Result<ArrayCOO<'c>, Error> =
    if uint64 m1.ncols <> uint64 m2.nrows then
        Error Error.InconsistentSizeOfArguments
    else
        let checkMultCondition (arr: COOEntry<'v>[]) getL getR =
            let rec loop i =
                if i < arr.Length then
                    let (_, _, v) = arr.[i]

                    if Option.isNone (op_mult (getL v) (getR v)) then
                        loop (i + 1)
                    else
                        false
                else
                    true

            loop 0

        let canOptimize =
            Option.isNone (op_mult None None)
            && checkMultCondition m1.entries (fun v -> Some v) (fun _ -> None)
            && checkMultCondition m2.entries (fun _ -> None) (fun v -> Some v)
            && uint64 m2.nrows <= uint64 System.Int32.MaxValue

        let generalResult () =
            let m1Map = m1.entries |> Array.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofArray
            let m2Map = m2.entries |> Array.map (fun (i, j, v) -> ((i, j), v)) |> Map.ofArray
            let kCount = uint64 m1.ncols

            let rec loopRow i acc =
                if i < uint64 m1.nrows then
                    let ri = i * 1UL<rowindex>

                    let rec loopCol j colAcc =
                        if j < uint64 m2.ncols then
                            let cj = j * 1UL<colindex>

                            let rec loopK k accK =
                                if k < kCount then
                                    let key1 = (ri, k * 1UL<colindex>)
                                    let key2 = (k * 1UL<rowindex>, cj)
                                    let accK' = op_add accK (op_mult (Map.tryFind key1 m1Map) (Map.tryFind key2 m2Map))
                                    loopK (k + 1UL) accK'
                                else
                                    accK

                            let colAcc' =
                                match loopK 0UL None with
                                | Some value -> (ri, cj, value) :: colAcc
                                | None -> colAcc

                            loopCol (j + 1UL) colAcc'
                        else
                            colAcc

                    let rowAcc = loopCol 0UL acc
                    loopRow (i + 1UL) rowAcc
                else
                    List.rev acc

            let finalArr = loopRow 0UL [] |> List.toArray
            ArrayCOO.CreatePresorted(m1.nrows, m2.ncols, finalArr)

        if canOptimize then
            let n1 = m1.entries.Length
            let n2 = m2.entries.Length
            let numRows = int (uint64 m2.nrows)

            let rowOffsets2 = Array.zeroCreate (numRows + 1)

            let rec countOffsets2 idx =
                if idx < n2 then
                    let (r, _, _) = m2.entries.[idx]
                    let rInt = int (uint64 r)
                    rowOffsets2.[rInt + 1] <- rowOffsets2.[rInt + 1] + 1
                    countOffsets2 (idx + 1)

            countOffsets2 0

            let rec prefixSum r =
                if r <= numRows then
                    rowOffsets2.[r] <- rowOffsets2.[r] + rowOffsets2.[r - 1]
                    prefixSum (r + 1)

            prefixSum 1

            let rec countPairs idx acc =
                if idx < n1 then
                    let (_, k, _) = m1.entries.[idx]
                    let kInt = int (uint64 k)
                    let count = rowOffsets2.[kInt + 1] - rowOffsets2.[kInt]
                    countPairs (idx + 1) (acc + count)
                else
                    acc

            let totalPairs = countPairs 0 0
            let products = Array.zeroCreate totalPairs

            let rec emitProducts idx writePos =
                if idx < n1 then
                    let (i, k, v1) = m1.entries.[idx]
                    let kInt = int (uint64 k)
                    let b = rowOffsets2.[kInt]
                    let e = rowOffsets2.[kInt + 1]

                    let rec loopQ q curWrite =
                        if q < e then
                            let (_, j, v2) = m2.entries.[q]

                            match op_mult (Some v1) (Some v2) with
                            | Some prod ->
                                products.[curWrite] <- (i, j, prod)
                                loopQ (q + 1) (curWrite + 1)
                            | None -> loopQ (q + 1) curWrite
                        else
                            curWrite

                    let nextWrite = loopQ b writePos
                    emitProducts (idx + 1) nextWrite
                else
                    writePos

            let actualProductsCount = emitProducts 0 0
            let effectiveProducts = truncate actualProductsCount products

            let canMerge =
                canMergeOpAdd op_add
                && Array.forall (fun (_, _, v) -> isAddNeutral op_add v) effectiveProducts

            if canMerge then
                Array.sortInPlaceWith entryCoordCompare effectiveProducts

                let target = Array.zeroCreate effectiveProducts.Length

                let rec reduceProducts q outIdx =
                    if q < effectiveProducts.Length then
                        let (i, j, v) = effectiveProducts.[q]

                        let rec sumDup qq curSum =
                            if qq < effectiveProducts.Length then
                                let (i', j', v') = effectiveProducts.[qq]

                                if i' = i && j' = j then
                                    sumDup (qq + 1) (op_add curSum (Some v'))
                                else
                                    qq, curSum
                            else
                                qq, curSum

                        let nextQ, finalSum = sumDup (q + 1) (Some v)

                        match finalSum with
                        | Some value ->
                            target.[outIdx] <- (i, j, value)
                            reduceProducts nextQ (outIdx + 1)
                        | None -> reduceProducts nextQ outIdx
                    else
                        outIdx

                let finalCount = reduceProducts 0 0
                let resultArr = truncate finalCount target

                ArrayCOO.CreatePresorted(m1.nrows, m2.ncols, resultArr) |> Ok
            else
                generalResult () |> Ok
        else
            generalResult () |> Ok
