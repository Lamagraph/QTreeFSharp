/// Linear algebra over COO vectors and matrices.
///
/// `op_mult` is applied only where both operands are stored, so the results equal the
/// quad-tree's whenever `op_mult` returns `None` for a missing operand (true for any semiring).
/// Sorted operations reduce the products of each output cell in ascending inner index, the
/// same order the quad-tree combines them, so even a non-commutative `op_add` gives the
/// quad-tree's result. `...Unsorted` operations reduce in input order and need a commutative
/// `op_add` for an order-independent result (see `LinearAlgebra.COO.Common`).
module LinearAlgebra.COO.Operations

open LinearAlgebra.Common
open LinearAlgebra.COO.Common

type Error =
    | InconsistentSizeOfArguments

    override this.ToString() =
        match this with
        | InconsistentSizeOfArguments -> "Inconsistent size of arguments"

type private Product<'a, 'b> = (uint64<index> * 'a) * Matrix.COOEntry<'b>

/// Pairs every vector entry with the matrix entries of the same row, by a linear merge.
/// Requires both inputs sorted; products come out ordered by (row, column).
let private joinSorted (vector: (uint64<index> * 'a) list) (matrix: Matrix.COOEntry<'b> list) : Product<'a, 'b> list =
    let rec go vs ms acc =
        match vs, ms with
        | [], _
        | _, [] -> List.rev acc
        | (i, a) :: vt, ((r, _, _) as entry) :: mt ->
            if uint64 i < uint64 r then go vt ms acc
            elif uint64 i > uint64 r then go vs mt acc
            else go vs mt (((i, a), entry) :: acc)

    go vector matrix []

/// Rows of `matrix`, each in the order it appears in the input.
let private rowsOf (matrix: Matrix.SparseMatrix<'b>) : Map<uint64<rowindex>, Matrix.COOEntry<'b> list> =
    matrix.data |> List.groupBy (fun (r, _, _) -> r) |> Map.ofList

let private rowOf rows (k: uint64) =
    rows |> Map.tryFind (k * 1UL<rowindex>) |> Option.defaultValue []

/// Pairs every vector entry with the matrix entries of the same row via a row index.
/// Works in any order; products follow the order of the vector.
let private joinUnsorted (vector: (uint64<index> * 'a) list) (matrix: Matrix.SparseMatrix<'b>) : Product<'a, 'b> list =
    let rows = rowsOf matrix

    vector
    |> List.collect (fun ((i, _) as entry) -> rowOf rows (uint64 i) |> List.map (fun e -> entry, e))

let private vxmiWith
    (products: Vector.SparseVector<'a> -> Matrix.SparseMatrix<'b> -> Product<'a, 'b> list)
    (finish: Vector.SparseVector<'c> -> Vector.SparseVector<'c>)
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: uint64<index> * 'a -> Matrix.COOEntry<'b> -> 'c option)
    (vector: Vector.SparseVector<'a>)
    (matrix: Matrix.SparseMatrix<'b>)
    : Result<Vector.SparseVector<'c>, Error> =
    if uint64 vector.length <> uint64 matrix.nrows then
        Error InconsistentSizeOfArguments
    else
        let data =
            products vector matrix
            |> List.choose (fun (v, ((_, c, _) as e)) -> op_mult v e |> Option.map (fun x -> uint64 c * 1UL<index>, x))
            |> groupReduce op_add

        Ok(
            finish
                { Vector.SparseVector.length = uint64 matrix.ncols * 1UL<dataLength>
                  data = data }
        )

let private sortedProducts (vector: Vector.SparseVector<'a>) (matrix: Matrix.SparseMatrix<'b>) =
    joinSorted vector.data matrix.data

let private unsortedProducts (vector: Vector.SparseVector<'a>) (matrix: Matrix.SparseMatrix<'b>) =
    joinUnsorted vector.data matrix

/// Vector-matrix product where `op_mult` also sees the coordinates of both operands.
let vxmi_values
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: uint64<index> * 'a -> uint64<rowindex> * uint64<colindex> * 'b -> 'c option)
    (vector: Vector.SparseVector<'a>)
    (matrix: Matrix.SparseMatrix<'b>)
    =
    vxmiWith sortedProducts Vector.sort op_add op_mult vector matrix

let vxmi_valuesUnsorted
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: uint64<index> * 'a -> uint64<rowindex> * uint64<colindex> * 'b -> 'c option)
    (vector: Vector.SparseVector<'a>)
    (matrix: Matrix.SparseMatrix<'b>)
    =
    vxmiWith unsortedProducts id op_add op_mult vector matrix

let private valuesOnly (op_mult: 'a option -> 'b option -> 'c option) (_, a) (_, _, b) = op_mult (Some a) (Some b)

let vxm
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: 'a option -> 'b option -> 'c option)
    (vector: Vector.SparseVector<'a>)
    (matrix: Matrix.SparseMatrix<'b>)
    =
    vxmi_values op_add (valuesOnly op_mult) vector matrix

let vxmUnsorted
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: 'a option -> 'b option -> 'c option)
    (vector: Vector.SparseVector<'a>)
    (matrix: Matrix.SparseMatrix<'b>)
    =
    vxmi_valuesUnsorted op_add (valuesOnly op_mult) vector matrix

/// Products of `matrix1`'s entries with the matching rows of `matrix2`, in `matrix1`'s order.
/// For a row-major `matrix1` the products of each output cell come in ascending inner index.
let private mxmWith
    (finish: Matrix.SparseMatrix<'c> -> Matrix.SparseMatrix<'c>)
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: 'a option -> 'b option -> 'c option)
    (matrix1: Matrix.SparseMatrix<'a>)
    (matrix2: Matrix.SparseMatrix<'b>)
    : Result<Matrix.SparseMatrix<'c>, Error> =
    if uint64 matrix1.ncols <> uint64 matrix2.nrows then
        Error InconsistentSizeOfArguments
    else
        let rows = rowsOf matrix2

        let data =
            matrix1.data
            |> List.collect (fun (i, k, a) ->
                rowOf rows (uint64 k)
                |> List.choose (fun (_, j, b) -> op_mult (Some a) (Some b) |> Option.map (fun x -> (i, j), x)))
            |> groupReduce op_add
            |> List.map (fun ((i, j), x) -> i, j, x)

        Ok(
            finish
                { Matrix.SparseMatrix.nrows = matrix1.nrows
                  ncols = matrix2.ncols
                  data = data }
        )

let mxm
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: 'a option -> 'b option -> 'c option)
    (matrix1: Matrix.SparseMatrix<'a>)
    (matrix2: Matrix.SparseMatrix<'b>)
    =
    mxmWith Matrix.sort op_add op_mult matrix1 matrix2

let mxmUnsorted
    (op_add: 'c option -> 'c option -> 'c option)
    (op_mult: 'a option -> 'b option -> 'c option)
    (matrix1: Matrix.SparseMatrix<'a>)
    (matrix2: Matrix.SparseMatrix<'b>)
    =
    mxmWith id op_add op_mult matrix1 matrix2
