module LinearAlgebra

open Vector
open Matrix

let rec multScalar op_add (x: uint) y =
    if x = 1u then
        y
    else
        let v = multScalar op_add (x / 2u) y
        let res = op_add v v
        if x % 2u = 0u then res else op_add res v

let vxm op_add op_mult (vector: SparseVector<'a>) (matrix: SparseMatrix<'b>) =
    if uint64 vector.length = uint64 matrix.nrows then
        let inner len vector matrix =
            match (vector, matrix) with
            | btree.Leaf(v1), qtree.Leaf(v2) ->
                let v = op_mult v1 v2
                btree.Leaf(multScalar op_add len v), len
            | btree.Leaf(v1), Node(t1, t2, t3, t4) ->
                //let new_t1, nvals1 = inner (len/2u) vector
                failwith "Unfinished..."

        Result.Failure "TODO..."
    else
        Result.Failure "The length of the vector must be equals to the number of rows of the matrix."
