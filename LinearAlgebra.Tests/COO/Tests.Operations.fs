module LinearAlgebra.COO.Operations.Tests

open Xunit
open FsCheck.Xunit

open LinearAlgebra.Common
open LinearAlgebra.COO.Tests.Helpers

module CVector = LinearAlgebra.COO.Vector
module CMatrix = LinearAlgebra.COO.Matrix
module COps = LinearAlgebra.COO.Operations
module QOps = LinearAlgebra.QuadTree.Operations

[<Fact>]
let ``vxm rejects inconsistent sizes`` () =
    let actual =
        COps.vxm plus times (CVector.empty 3UL<dataLength>) (CMatrix.empty 4UL<nrows> 3UL<ncols>)

    Assert.Equal(Error COps.InconsistentSizeOfArguments, actual)
    Assert.Equal("Inconsistent size of arguments", string COps.InconsistentSizeOfArguments)

[<Fact>]
let ``mxm rejects inconsistent sizes`` () =
    let actual =
        COps.mxm plus times (CMatrix.empty 2UL<nrows> 3UL<ncols>) (CMatrix.empty 2UL<nrows> 3UL<ncols>)

    Assert.Equal(Error COps.InconsistentSizeOfArguments, actual)

/// Frontier {1, 3}, edges 1 -> 2 and 3 -> 2; `op_mult` yields the source vertex (as in `bfsParent`).
let private parentOf2 (frontier: (uint64<index> * int) list) vxmi =
    let graph =
        cMatrix 4UL 4UL [ 1UL<rowindex>, 2UL<colindex>, 1; 3UL<rowindex>, 2UL<colindex>, 1 ]

    let v: CVector.SparseVector<int> =
        { length = 4UL<dataLength>
          data = frontier }

    let result: CVector.SparseVector<uint64> =
        vxmi first (fun (i: uint64<index>, _) _ -> Some(uint64 i)) v graph |> unwrap

    CVector.unsafeGet result 2UL<index>

/// Pitfall: with a non-commutative `op_add`, the unsorted reduction depends on the input order.
[<Fact>]
let ``vxmi_valuesUnsorted with first-wins op_add depends on order (documented pitfall)`` () =
    Assert.Equal(Some 1UL, parentOf2 [ 1UL<index>, 0; 3UL<index>, 0 ] COps.vxmi_values)
    Assert.Equal(Some 1UL, parentOf2 [ 1UL<index>, 0; 3UL<index>, 0 ] COps.vxmi_valuesUnsorted)
    Assert.Equal(Some 3UL, parentOf2 [ 3UL<index>, 0; 1UL<index>, 0 ] COps.vxmi_valuesUnsorted)

let private indexedMult (i: uint64<index>, x: int) (r: uint64<rowindex>, c: uint64<colindex>, y: int) =
    if (x + y) % 3 = 0 then
        None
    else
        Some(int i * 100 + int c * 10 + x * y + int r)

[<Property>]
let ``vxm equals quad-tree`` (r: byte) (c: byte) (ev: (byte * int) list) (em: (byte * byte * int) list) =
    let nrows, ncols = dim r, dim c
    let dv, dm = vectorData nrows ev, matrixData nrows ncols em

    let expected =
        QOps.vxm plus times (qVector nrows dv) (qMatrix nrows ncols dm)
        |> unwrap
        |> qVectorData

    let actual =
        COps.vxm plus times (cVector nrows dv) (cMatrix nrows ncols dm) |> unwrap

    expected = actual.data

/// A first-wins `op_add` also pins down the reduction order: it must match the quad-tree's.
[<Property>]
let ``vxmi_values equals quad-tree even for non-commutative op_add``
    (r: byte)
    (c: byte)
    (ev: (byte * int) list)
    (em: (byte * byte * int) list)
    =
    let nrows, ncols = dim r, dim c
    let dv, dm = vectorData nrows ev, matrixData nrows ncols em

    let expected =
        QOps.vxmi_values first indexedMult (qVector nrows dv) (qMatrix nrows ncols dm)
        |> unwrap
        |> qVectorData

    let actual =
        COps.vxmi_values first indexedMult (cVector nrows dv) (cMatrix nrows ncols dm)
        |> unwrap

    expected = actual.data

[<Property>]
let ``mxm equals quad-tree``
    (r: byte)
    (k: byte)
    (c: byte)
    (e1: (byte * byte * int) list)
    (e2: (byte * byte * int) list)
    =
    let nrows, inner, ncols = dim r, dim k, dim c
    let d1, d2 = matrixData nrows inner e1, matrixData inner ncols e2

    let expected =
        QOps.mxm plus times (qMatrix nrows inner d1) (qMatrix inner ncols d2)
        |> unwrap
        |> qMatrixData

    let actual =
        COps.mxm plus times (cMatrix nrows inner d1) (cMatrix inner ncols d2) |> unwrap

    expected = actual.data

[<Property>]
let ``mxm with first-wins op_add equals quad-tree``
    (n: byte)
    (e1: (byte * byte * int) list)
    (e2: (byte * byte * int) list)
    =
    let size = dim n
    let d1, d2 = matrixData size size e1, matrixData size size e2

    let expected =
        QOps.mxm first times (qMatrix size size d1) (qMatrix size size d2)
        |> unwrap
        |> qMatrixData

    let actual =
        COps.mxm first times (cMatrix size size d1) (cMatrix size size d2) |> unwrap

    expected = actual.data

[<Property>]
let ``unsorted twins equal sorted ops after sort for commutative op_add``
    (r: byte)
    (c: byte)
    (ev: (byte * int) list)
    (em: (byte * byte * int) list)
    (em2: (byte * byte * int) list)
    (seed: int)
    =
    let nrows, ncols = dim r, dim c
    let v = cVector nrows (vectorData nrows ev)
    let m = cMatrix nrows ncols (matrixData nrows ncols em)
    let m2 = cMatrix ncols nrows (matrixData ncols nrows em2)

    let sv, sm, sm2 =
        shuffleVector seed v, shuffleMatrix (seed + 1) m, shuffleMatrix (seed + 2) m2

    let minIndexed (i: uint64<index>, _) (_, _, _) = Some(int i)

    let minAdd x y =
        match x, y with
        | Some a, Some b -> Some(min a b)
        | _ -> plus x y

    CVector.sort (COps.vxmUnsorted plus times sv sm |> unwrap) = (COps.vxm plus times v m |> unwrap)
    && CVector.sort (COps.vxmi_valuesUnsorted minAdd minIndexed sv sm |> unwrap) = (COps.vxmi_values
                                                                                        minAdd
                                                                                        minIndexed
                                                                                        v
                                                                                        m
                                                                                    |> unwrap)
    && CMatrix.sort (COps.mxmUnsorted plus times sm sm2 |> unwrap) = (COps.mxm plus times m m2 |> unwrap)
