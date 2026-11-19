/// Definitions shared by every representation, chiefly the units of measure that
/// distinguish dimensions from indices. Algorithms (`Graph.*`) take their units from
/// here, so they never depend on a concrete representation.
module LinearAlgebra.Common

/// Number of stored (non-empty) values.
[<Measure>]
type nvals

/// Logical length of a vector.
[<Measure>]
type dataLength

/// Index into a vector.
[<Measure>]
type index

/// Number of rows of a matrix.
[<Measure>]
type nrows

/// Number of columns of a matrix.
[<Measure>]
type ncols

/// Row index into a matrix.
[<Measure>]
type rowindex

/// Column index into a matrix.
[<Measure>]
type colindex
