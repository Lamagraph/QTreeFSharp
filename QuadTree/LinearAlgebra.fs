module LinearAlgebra

open Vector
open Matrix
open Common

let rec multScalar op_add (x: uint64) y =
    if x = 1UL then
        y
    else
        let v = multScalar op_add (x / 2UL) y
        let res = op_add v v
        if x % 2UL = 0UL then res else op_add res v

(*
let vxm op_add op_mult (vector: SparseVector<'a>) (matrix: SparseMatrix<'b>) =

    if uint64 vector.length = uint64 matrix.nrows then
        let rec inner size vector matrix =
            match (vector, matrix) with
            | btree.Leaf(UserValue(v1)), qtree.Leaf(UserValue(v2)) ->
                let res = multScalar op_add (uint64 size) (op_mult v1 v2)
                let nnz = 
                    match res with 
                    | None -> 0UL<nvals>
                    | _ -> (uint64 size) * 1UL<nvals>
                        

                btree.Leaf(UserValue(res)), nnz 

            | btree.Leaf(UserValue(_)), qtree.Node(t1, t2, t3, t4) ->
                let new_t1, nvals1 = inner (size / 2UL) vector t1 
                let new_t2, nvals2 = inner (size / 2UL) vector t3 
                (Vector.mkNode new_t1 new_t2), nvals1 + nvals2

            | btree.Node(t1, t2), qtree.Leaf(UserValue(_)) ->
                let new_t1, nvals1 = inner (size / 2UL) t1 matrix
                let new_t2, nvals2 = inner (size / 2UL) t2 matrix
                (Vector.mkNode new_t1 new_t2), nvals1 + nvals2

            | btree.Node(t1, t2), qtree.Node(t1, t2, t3, t4) ->
                let new_t1, nvals1 = inner (size / 2UL) t1 matrix
                let new_t2, nvals2 = inner (size / 2UL) t2 matrix
                (Vector.mkNode new_t1 new_t2), nvals1 + nvals2
                

        Result.Failure "TODO..."
    else
        Result.Failure "The length of the vector must be equals to the number of rows of the matrix."
*)
