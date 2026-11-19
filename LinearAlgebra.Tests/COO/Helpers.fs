/// Builders shared by the COO tests: the same random input as a quad-tree and as a COO value,
/// so properties can check that both representations agree.
module LinearAlgebra.COO.Tests.Helpers

open System
open LinearAlgebra.Common

module QVector = LinearAlgebra.QuadTree.Vector
module QMatrix = LinearAlgebra.QuadTree.Matrix
module CVector = LinearAlgebra.COO.Vector
module CMatrix = LinearAlgebra.COO.Matrix

let unwrap (result: Result<'a, 'e>) =
    match result with
    | Ok x -> x
    | Error e -> failwithf "Unexpected error: %O" e

/// Small dimension in 1..12 from an arbitrary byte.
let dim (n: byte) = uint64 (int n % 12 + 1)

let vectorData (length: uint64) (entries: (byte * int) list) =
    entries
    |> List.map (fun (i, v) -> (uint64 i % length) * 1UL<index>, v)
    |> List.distinctBy fst

let matrixData (nrows: uint64) (ncols: uint64) (entries: (byte * byte * 'a) list) =
    entries
    |> List.map (fun (r, c, v) -> (uint64 r % nrows) * 1UL<rowindex>, (uint64 c % ncols) * 1UL<colindex>, v)
    |> List.distinctBy (fun (r, c, _) -> r, c)

let qVector (length: uint64) data =
    QVector.CoordinateList(length * 1UL<dataLength>, data)
    |> QVector.fromCoordinateList
    |> unwrap

let cVector (length: uint64) data =
    CVector.CoordinateList(length * 1UL<dataLength>, data)
    |> CVector.fromCoordinateList
    |> unwrap

let qMatrix (nrows: uint64) (ncols: uint64) data =
    QMatrix.CoordinateList(nrows * 1UL<nrows>, ncols * 1UL<ncols>, data)
    |> QMatrix.fromCoordinateList
    |> unwrap

let cMatrix (nrows: uint64) (ncols: uint64) data =
    CMatrix.CoordinateList(nrows * 1UL<nrows>, ncols * 1UL<ncols>, data)
    |> CMatrix.fromCoordinateList
    |> unwrap

/// Entries of a quad-tree vector, sorted by index.
let qVectorData (vector: QVector.SparseVector<'a>) = (QVector.toCoordinateList vector).data

/// Entries of a quad-tree matrix, sorted row-major.
let qMatrixData (matrix: QMatrix.SparseMatrix<'a>) = (QMatrix.toCoordinateList matrix).list

/// Deterministic permutation driven by `seed`.
let shuffle (seed: int) (list: 'a list) =
    let random = Random(seed)
    list |> List.map (fun x -> random.Next(), x) |> List.sortBy fst |> List.map snd

let shuffleVector seed (vector: CVector.SparseVector<'a>) : CVector.SparseVector<'a> =
    { vector with
        data = shuffle seed vector.data }

let shuffleMatrix seed (matrix: CMatrix.SparseMatrix<'a>) : CMatrix.SparseMatrix<'a> =
    { matrix with
        data = shuffle seed matrix.data }

/// Monoid-like `+` over options: commutative and associative.
let plus (x: int option) (y: int option) =
    match x, y with
    | Some a, Some b -> Some(a + b)
    | Some a, None
    | None, Some a -> Some a
    | None, None -> None

/// Semiring multiplication: `None` annihilates.
let times (x: int option) (y: int option) =
    match x, y with
    | Some a, Some b -> Some(a * b)
    | _ -> None

/// "First value wins": associative but not commutative.
let first (x: 'a option) (y: 'a option) =
    match x with
    | Some _ -> x
    | None -> y
