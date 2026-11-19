/// Sparse matrix in coordinate (COO) format: an immutable list of `(row, column, value)` triples.
/// Only present values are stored; there is neither `Dummy` nor a stored `Option`.
///
/// Sorted form (all operations without a suffix): `data` is sorted row-major by `(row, column)`,
/// coordinates are unique and in range. `...Unsorted` operations accept and return any order
/// (coordinates still unique), so a chain of them can skip intermediate sorting; `sort` returns
/// to the sorted form. See `LinearAlgebra.COO.Common` for the pitfalls of mixing the two forms.
module LinearAlgebra.COO.Matrix

open LinearAlgebra.Common
open LinearAlgebra.COO.Common

type COOEntry<'value> = uint64<rowindex> * uint64<colindex> * 'value

/// Unvalidated input for `fromCoordinateList`: any order, duplicates allowed (last one wins).
[<Struct>]
type CoordinateList<'value> =
    val nrows: uint64<nrows>
    val ncols: uint64<ncols>
    val list: COOEntry<'value> list

    new(_nrows, _ncols, _list) =
        { nrows = _nrows
          ncols = _ncols
          list = _list }

type SparseMatrix<'value> =
    { nrows: uint64<nrows>
      ncols: uint64<ncols>
      data: COOEntry<'value> list }

type Error =
    | InconsistentSizeOfArguments
    | CoordinatesOutOfRange

    override this.ToString() =
        match this with
        | InconsistentSizeOfArguments -> "Inconsistent size of arguments"
        | CoordinatesOutOfRange -> "Coordinates out of range"

let private toKeyed (data: COOEntry<'a> list) =
    data |> List.map (fun (r, c, v) -> (r, c), v)

let private ofKeyed (data: ((uint64<rowindex> * uint64<colindex>) * 'a) list) : COOEntry<'a> list =
    data |> List.map (fun ((r, c), v) -> r, c, v)

let nvals (matrix: SparseMatrix<'a>) : uint64<nvals> =
    (uint64 (List.length matrix.data)) * 1UL<nvals>

let empty (nrows: uint64<nrows>) (ncols: uint64<ncols>) : SparseMatrix<'a> =
    { nrows = nrows
      ncols = ncols
      data = [] }

/// Brings a matrix in any order into the sorted (row-major) form.
let sort (matrix: SparseMatrix<'a>) : SparseMatrix<'a> =
    { matrix with
        data = List.sortBy (fun (r, c, _) -> r, c) matrix.data }

/// Validates and deduplicates (last value wins) without sorting.
let fromCoordinateListUnsorted (coo: CoordinateList<'a>) : Result<SparseMatrix<'a>, Error> =
    if
        coo.list
        |> List.exists (fun (r, c, _) -> uint64 r >= uint64 coo.nrows || uint64 c >= uint64 coo.ncols)
    then
        Error CoordinatesOutOfRange
    else
        Ok
            { nrows = coo.nrows
              ncols = coo.ncols
              data = coo.list |> toKeyed |> dedupLast |> ofKeyed }

let fromCoordinateList (coo: CoordinateList<'a>) : Result<SparseMatrix<'a>, Error> =
    fromCoordinateListUnsorted coo |> Result.map sort

/// Works for both forms; the order of the list is preserved.
let toCoordinateList (matrix: SparseMatrix<'a>) : CoordinateList<'a> =
    CoordinateList(matrix.nrows, matrix.ncols, matrix.data)

let private allCoordinates (nrows: uint64<nrows>) (ncols: uint64<ncols>) () =
    List.init (int nrows) (fun r -> List.init (int ncols) (fun c -> uint64 r * 1UL<rowindex>, uint64 c * 1UL<colindex>))
    |> List.concat

let private map2With union (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) f =
    if matrix1.nrows <> matrix2.nrows || matrix1.ncols <> matrix2.ncols then
        Error InconsistentSizeOfArguments
    else
        Ok
            { nrows = matrix1.nrows
              ncols = matrix1.ncols
              data =
                union (toKeyed matrix1.data) (toKeyed matrix2.data)
                |> applyUnion (allCoordinates matrix1.nrows matrix1.ncols) f
                |> ofKeyed }

/// Element-wise `f` over the union of stored cells. If `f None None` is `Some`, the
/// remaining cells are filled with it, so the result equals the quad-tree's `map2`.
let map2
    (matrix1: SparseMatrix<'a>)
    (matrix2: SparseMatrix<'b>)
    (f: 'a option -> 'b option -> 'c option)
    : Result<SparseMatrix<'c>, Error> =
    map2With mergeUnion matrix1 matrix2 f

let map2Unsorted
    (matrix1: SparseMatrix<'a>)
    (matrix2: SparseMatrix<'b>)
    (f: 'a option -> 'b option -> 'c option)
    : Result<SparseMatrix<'c>, Error> =
    map2With joinUnion matrix1 matrix2 f

/// Keeps the values of `matrix1` where `f` holds for the corresponding cell of `matrix2`.
let mask (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) (f: 'b option -> bool) =
    map2 matrix1 matrix2 (fun m1 m2 -> if f m2 then m1 else None)

let maskUnsorted (matrix1: SparseMatrix<'a>) (matrix2: SparseMatrix<'b>) (f: 'b option -> bool) =
    map2Unsorted matrix1 matrix2 (fun m1 m2 -> if f m2 then m1 else None)

/// Lower triangle including the diagonal (`row >= column`), like the quad-tree's version.
/// A filter, so it keeps the order of its input.
let getLowerTriangleUnsorted (matrix: SparseMatrix<'a>) : SparseMatrix<'a> =
    { matrix with
        data = matrix.data |> List.filter (fun (r, c, _) -> uint64 r >= uint64 c) }

let getLowerTriangle (matrix: SparseMatrix<'a>) : SparseMatrix<'a> = getLowerTriangleUnsorted matrix

/// Swaps coordinates in O(nvals); the result is column-major, i.e. unsorted.
let transposeUnsorted (matrix: SparseMatrix<'a>) : SparseMatrix<'a> =
    { nrows = uint64 matrix.ncols * 1UL<nrows>
      ncols = uint64 matrix.nrows * 1UL<ncols>
      data =
        matrix.data
        |> List.map (fun (r, c, v) -> uint64 c * 1UL<rowindex>, uint64 r * 1UL<colindex>, v) }

let transpose (matrix: SparseMatrix<'a>) : SparseMatrix<'a> = matrix |> transposeUnsorted |> sort

/// Folds `folder` over the stored values in row-major order.
let foldAssociative (folder: 'a option -> 'a option -> 'a option) (state: 'a option) (matrix: SparseMatrix<'a>) =
    matrix.data |> List.fold (fun acc (_, _, v) -> folder acc (Some v)) state

/// Folds in list order: `folder` must be associative and commutative for an order-independent result.
let foldAssociativeUnsorted
    (folder: 'a option -> 'a option -> 'a option)
    (state: 'a option)
    (matrix: SparseMatrix<'a>)
    =
    foldAssociative folder state matrix
