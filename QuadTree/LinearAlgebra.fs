module LinearAlgebra

open Common

type Error<'value1, 'value2, 'value3> =
    | InconsistentStructureOfStorages of Vector.btree<Option<'value1>> * Matrix.qtree<Option<'value2>>
    | InconsistentSizeOfArguments of Vector.SparseVector<'value1> * Matrix.SparseMatrix<'value2>
    | VectorAdditionProblem of Vector.Error<'value3, 'value3>

let rec multScalar op_add (x: uint64) y =
    if x = 1UL then
        y
    else
        let v = multScalar op_add (x / 2UL) y
        let res = op_add v v
        if x % 2UL = 0UL then res else op_add res v


let vxm op_add op_mult (vector: Vector.SparseVector<'a>) (matrix: Matrix.SparseMatrix<'b>) =

    let rec inner (size: uint64<Vector.storageSize>) vector matrix =
        let _do x1 x2 y1 y2 y3 y4 =
            let new_size = size / 2UL

            match (inner new_size x1 y1), (inner new_size x1 y2), (inner new_size x2 y3), (inner new_size x2 y4) with
            | Result.Success((t1, nvals1)),
              Result.Success((t2, nvals2)),
              Result.Success((t3, nvals3)),
              Result.Success((t4, nvals4)) ->
                let data_length = (uint64 new_size) * 1UL<Vector.dataLength>
                let v1 = Vector.SparseVector(data_length, nvals1, (Vector.Storage(new_size, t1)))
                let v2 = Vector.SparseVector(data_length, nvals2, (Vector.Storage(new_size, t2)))
                let v3 = Vector.SparseVector(data_length, nvals3, (Vector.Storage(new_size, t3)))
                let v4 = Vector.SparseVector(data_length, nvals4, (Vector.Storage(new_size, t4)))

                let vAdd v1 (v2: Vector.SparseVector<_>) =
                    match v2.storage.data with
                    | Vector.Leaf(Dummy) -> Result.Success(v1)
                    | _ -> Vector.map2 v1 v2 op_add

                let z1 = vAdd v1 v3
                let z2 = vAdd v2 v4

                match (z1, z2) with
                | Result.Success(v1), Result.Success(v2) ->
                    Result.Success((Vector.mkNode v1.storage.data v2.storage.data), v1.nvals + v2.nvals)
                | Result.Failure(e), _
                | _, Result.Failure(e) -> Result.Failure(VectorAdditionProblem(e))

            | Result.Failure(e), _, _, _
            | _, Result.Failure(e), _, _
            | _, _, Result.Failure(e), _
            | _, _, _, Result.Failure(e) -> Result.Failure(e)

        match (vector, matrix) with
        | Vector.btree.Leaf(UserValue(v1)), Matrix.qtree.Leaf(UserValue(v2)) ->
            let res = multScalar op_add (uint64 size) (op_mult v1 v2)

            let nnz =
                match res with
                | None -> 0UL<nvals>
                | _ -> (uint64 size) * 1UL<nvals>

            Result.Success(Vector.btree.Leaf(UserValue(res)), nnz)

        | Vector.btree.Leaf(UserValue(_)), Matrix.qtree.Node(y1, y2, y3, y4) -> _do vector vector y1 y2 y3 y4
        | Vector.btree.Node(x1, x2), Matrix.qtree.Leaf(UserValue(_)) -> _do x1 x2 matrix matrix matrix matrix
        | Vector.btree.Node(x1, x2), Matrix.qtree.Node(y1, y2, y3, y4) -> _do x1 x2 y1 y2 y3 y4

        | Vector.btree.Leaf(Dummy), Matrix.qtree.Leaf(Dummy)
        | _, Matrix.qtree.Leaf(Dummy) -> Result.Success(Vector.btree.Leaf(Dummy), 0UL<nvals>)
        | (x, y) -> Result.Failure <| Error.InconsistentStructureOfStorages(x, y)

    if uint64 vector.length = uint64 matrix.nrows then
        let vector_storage =
            if uint64 vector.storage.size < uint64 matrix.storage.hSize then
                let rec increaseStorage storage_data (current_size: uint64<Vector.storageSize>) bound =
                    if current_size = bound then
                        storage_data
                    else
                        increaseStorage
                            (Vector.btree.Node(storage_data, Vector.btree.Leaf(Dummy)))
                            (current_size * 2UL)
                            bound

                let target_size = uint64 matrix.storage.hSize * 1UL<Vector.storageSize>
                Vector.Storage(target_size, increaseStorage vector.storage.data vector.storage.size target_size)
            else
                vector.storage

        match inner vector_storage.size vector_storage.data matrix.storage.data with
        | Result.Failure x -> Result.Failure x
        | Result.Success(storage, nvals) ->
            (Vector.SparseVector(
                (uint64 matrix.ncols) * 1UL<Vector.dataLength>,
                nvals,
                (Vector.Storage((uint64 matrix.storage.hSize) * 1UL<Vector.storageSize>, storage))
            ))
            |> Result.Success
    else
        (Error.InconsistentSizeOfArguments(vector, matrix)) |> Result.Failure
