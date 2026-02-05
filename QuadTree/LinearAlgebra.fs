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

    let rec inner (size: uint64<storageSize>) vector matrix =
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
            if uint64 vector.storage.size < uint64 matrix.storage.size then
                let rec increaseStorage storage_data (current_size: uint64<storageSize>) bound =
                    if current_size = bound then
                        storage_data
                    else
                        increaseStorage
                            (Vector.btree.Node(storage_data, Vector.btree.Leaf(Dummy)))
                            (current_size * 2UL)
                            bound

                let target_size = matrix.storage.size
                Vector.Storage(target_size, increaseStorage vector.storage.data vector.storage.size target_size)
            else
                vector.storage

        match inner vector_storage.size vector_storage.data matrix.storage.data with
        | Result.Failure x -> Result.Failure x
        | Result.Success(storage, nvals) ->
            (Vector.SparseVector(
                (uint64 matrix.ncols) * 1UL<Vector.dataLength>,
                nvals,
                (Vector.Storage(matrix.storage.size, storage))
            ))
            |> Result.Success
    else
        (Error.InconsistentSizeOfArguments(vector, matrix)) |> Result.Failure



let mxm op_add op_mult (m1: Matrix.SparseMatrix<'a>) (m2: Matrix.SparseMatrix<'a>) =

    let rec shrink tree (size: uint64<storageSize>) =
        match tree with
        | Matrix.qtree.Node(nw, ne, sw, _) when ne = sw && ne = Matrix.qtree.Leaf Dummy -> shrink nw (size / 2UL)
        | _ -> tree, size

    let rec expand tree expandRatio =
        match expandRatio with
        | 1UL<storageSize> -> tree
        | _ ->
            expand
                (Matrix.mkNode tree (Matrix.qtree.Leaf Dummy) (Matrix.qtree.Leaf Dummy) (Matrix.qtree.Leaf Dummy))
                (expandRatio / 2UL)

    let rec multiply size m1 m2 =
        let divided (nw1, ne1, sw1, se1) (nw2, ne2, sw2, se2) =
            let halfSize = size / 2UL

            // Double check this code
            let nw1xnw2, ne1xsw2, nw1xne2, ne1xse2, sw1xnw2, se1xsw2, sw1xne2, se1xse2 =
                (multiply halfSize nw1 nw2),
                (multiply halfSize ne1 sw2),
                (multiply halfSize nw1 ne2),
                (multiply halfSize ne1 se2),
                (multiply halfSize sw1 nw2),
                (multiply halfSize se1 sw2),
                (multiply halfSize sw1 ne2),
                (multiply halfSize se1 se2)

            match nw1xnw2, ne1xsw2, nw1xne2, ne1xse2, sw1xnw2, se1xsw2, sw1xne2, se1xse2 with
            | Result.Success(tnw1xnw2, nvals_nw1xnw2),
              Result.Success(tne1xsw2, nvals_ne1xsw2),
              Result.Success(tnw1xne2, nvals_nw1xne2),
              Result.Success(tne1xse2, nvals_ne1xse2),
              Result.Success(tsw1xnw2, nvals_sw1xnw2),
              Result.Success(tse1xsw2, nvals_se1xsw2),
              Result.Success(tsw1xne2, nvals_sw1xne2),
              Result.Success(tse1xse2, nvals_se1xse2) ->
                let nrows = halfSize * 1UL<Matrix.nrows>
                let ncols = halfSize * 1UL<Matrix.ncols>
                let storageSize = halfSize * 1UL<storageSize>

                let nw1xnw2 =
                    Matrix.SparseMatrix(nrows, ncols, nvals_nw1xnw2, Matrix.Storage(storageSize, tnw1xnw2))

                let ne1xsw2 =
                    Matrix.SparseMatrix(nrows, ncols, nvals_ne1xsw2, Matrix.Storage(storageSize, tne1xsw2))

                let nw1xne2 =
                    Matrix.SparseMatrix(nrows, ncols, nvals_nw1xne2, Matrix.Storage(storageSize, tnw1xne2))

                let ne1xse2 =
                    Matrix.SparseMatrix(nrows, ncols, nvals_ne1xse2, Matrix.Storage(storageSize, tne1xse2))

                let sw1xnw2 =
                    Matrix.SparseMatrix(nrows, ncols, nvals_sw1xnw2, Matrix.Storage(storageSize, tsw1xnw2))

                let se1xsw2 =
                    Matrix.SparseMatrix(nrows, ncols, nvals_se1xsw2, Matrix.Storage(storageSize, tse1xsw2))

                let sw1xne2 =
                    Matrix.SparseMatrix(nrows, ncols, nvals_sw1xne2, Matrix.Storage(storageSize, tsw1xne2))

                let se1xse2 =
                    Matrix.SparseMatrix(nrows, ncols, nvals_se1xse2, Matrix.Storage(storageSize, tse1xse2))

                let mAdd m1 (m2: Matrix.SparseMatrix<_>) =
                    match m2.storage.data with
                    | Matrix.qtree.Leaf Dummy -> Result.Success m1
                    | _ -> Matrix.map2 m1 m2 op_add

                let rnw = mAdd nw1xnw2 ne1xsw2
                let rne = mAdd nw1xne2 ne1xse2
                let rsw = mAdd sw1xnw2 se1xsw2
                let rse = mAdd sw1xne2 se1xse2

                match rnw, rne, rsw, rse with
                | Result.Success(nw), Result.Success(ne), Result.Success(sw), Result.Success(se) ->
                    Result.Success(
                        Matrix.mkNode nw.storage.data ne.storage.data sw.storage.data se.storage.data,
                        nw.nvals + ne.nvals + sw.nvals + se.nvals
                    )
                | Result.Failure(e), _, _, _
                | _, Result.Failure(e), _, _
                | _, _, Result.Failure(e), _
                | _, _, _, Result.Failure(e) -> Result.Failure(e)

            | Result.Failure(e), _, _, _, _, _, _, _
            | _, Result.Failure(e), _, _, _, _, _, _
            | _, _, Result.Failure(e), _, _, _, _, _
            | _, _, _, Result.Failure(e), _, _, _, _
            | _, _, _, _, Result.Failure(e), _, _, _
            | _, _, _, _, _, Result.Failure(e), _, _
            | _, _, _, _, _, _, Result.Failure(e), _
            | _, _, _, _, _, _, _, Result.Failure(e) -> Result.Failure(e)

        match m1, m2 with
        | Matrix.qtree.Leaf(UserValue v1), Matrix.qtree.Leaf(UserValue v2) ->
            let res = multScalar op_add (uint64 size) (op_mult v1 v2)

            let nnz =
                match res with
                | None -> 0UL<nvals>
                | _ -> size * size * 1UL<nvals>

            Result.Success(Matrix.qtree.Leaf(UserValue res), nnz)
        | Matrix.qtree.Leaf(UserValue(_)), Matrix.qtree.Node(nw2, ne2, sw2, se2) ->
            divided (m1, m1, m1, m1) (nw2, ne2, sw2, se2)
        | Matrix.qtree.Node(nw1, ne1, sw1, se1), Matrix.qtree.Leaf(UserValue(_)) ->
            divided (nw1, ne1, sw1, se1) (m2, m2, m2, m2)
        | Matrix.qtree.Node(nw1, ne1, sw1, se1), Matrix.qtree.Node(nw2, ne2, sw2, se2) ->
            divided (nw1, ne1, sw1, se1) (nw2, ne2, sw2, se2)
        | Matrix.qtree.Leaf Dummy, _
        | _, Matrix.qtree.Leaf Dummy -> Result.Success(Matrix.qtree.Leaf Dummy, 0UL<nvals>)

    if uint64 m1.ncols = uint64 m2.nrows then
        let nrows = m1.nrows
        let ncols = m2.ncols
        let storageSize = max m1.storage.size m2.storage.size

        let m1_tree, m2_tree =
            if m1.storage.size < m2.storage.size then
                expand (m1.storage.data) (m2.storage.size / (uint64 m1.storage.size)), m2.storage.data
            else if m1.storage.size > m2.storage.size then
                m1.storage.data, expand (m2.storage.data) (m1.storage.size / (uint64 m2.storage.size))
            else
                m1.storage.data, m2.storage.data

        match multiply (uint64 storageSize) m1_tree m2_tree with
        | Result.Success(tree, nvals) ->
            // in case the resulting storageSize can be smaller
            // e.g. (2x3) * (3x2) matrices
            let tree, storageSize = shrink tree storageSize
            Result.Success(Matrix.SparseMatrix(nrows, ncols, nvals, Matrix.Storage(storageSize, tree)))
        | Result.Failure(e) -> Result.Failure(e)
    else
        Matrix.Error.InconsistentSizeOfArguments(m1, m2) |> Result.Failure
