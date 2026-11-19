/// BFS, SSSP and TriangleCount on the COO representation: fixed scenarios plus properties
/// checking that COO and the quad-tree agree on random graphs.
module Graph.COO.Tests

open Xunit
open FsCheck.Xunit

open LinearAlgebra.Common
open LinearAlgebra.COO

module QVector = LinearAlgebra.QuadTree.Vector
module QMatrix = LinearAlgebra.QuadTree.Matrix
module QBFS = Graph.Impl.QuadTree.BFS
module QSSSP = Graph.Impl.QuadTree.SSSP
module QTC = Graph.Impl.QuadTree.TriangleCount
module CBFS = Graph.Impl.COO.BFS
module CSSSP = Graph.Impl.COO.SSSP
module CTC = Graph.Impl.COO.TriangleCount

let private unwrap (result: Result<'a, 'e>) =
    match result with
    | Ok x -> x
    | Error e -> failwithf "Unexpected error: %O" e

let private graph n (edges: (uint64 * uint64 * 'a) list) =
    Matrix.CoordinateList(
        n * 1UL<nrows>,
        n * 1UL<ncols>,
        edges |> List.map (fun (r, c, v) -> r * 1UL<rowindex>, c * 1UL<colindex>, v)
    )
    |> Matrix.fromCoordinateList
    |> unwrap

let private start n s =
    Vector.CoordinateList(n * 1UL<dataLength>, [ s * 1UL<index>, 1UL ])
    |> Vector.fromCoordinateList
    |> unwrap

let private dense n (vector: Vector.SparseVector<'a>) =
    List.init (int n) (fun i -> Vector.unsafeGet vector (uint64 i * 1UL<index>))

let private line3 =
    graph 3UL [ 0UL, 1UL, 1UL; 1UL, 0UL, 1UL; 1UL, 2UL, 1UL; 2UL, 1UL, 1UL ]

[<Fact>]
let ``Level bfs 3 node line start 0`` () =
    Assert.Equal<uint64 option list>(
        [ Some 0UL; Some 1UL; Some 2UL ],
        CBFS.bfsLevel line3 (start 3UL 0UL) |> unwrap |> dense 3UL
    )

[<Fact>]
let ``Parent bfs 3 node line start 1`` () =
    Assert.Equal<uint64 option list>(
        [ Some 1UL; Some 1UL; Some 1UL ],
        CBFS.bfsParent line3 (start 3UL 1UL) |> unwrap |> dense 3UL
    )

[<Fact>]
let ``SSSP on a weighted line`` () =
    let g = graph 3UL [ 0UL, 1UL, 2.0; 1UL, 2UL, 3.0; 0UL, 2UL, 10.0 ]
    Assert.Equal<float option list>([ Some 0.0; Some 2.0; Some 5.0 ], CSSSP.sssp g 0UL |> unwrap |> dense 3UL)

let private sevenV =
    let lower =
        [ 1UL, 0UL
          3UL, 0UL
          3UL, 1UL
          3UL, 2UL
          4UL, 1UL
          5UL, 2UL
          5UL, 3UL
          5UL, 4UL
          6UL, 1UL
          6UL, 2UL
          6UL, 3UL
          6UL, 4UL ]

    graph 7UL (lower |> List.collect (fun (r, c) -> [ r, c, (); c, r, () ]))

[<Fact>]
let ``7V Triangle count`` () =
    Assert.Equal(Ok(Some 5UL), CTC.triangleCount sevenV)

let private tcAdd (x: uint64 option) (y: uint64 option) =
    match x, y with
    | Some a, Some b -> Some(a + b)
    | Some a, None
    | None, Some a -> Some a
    | None, None -> None

let private tcMult (x: 'a option) (y: 'a option) =
    match x, y with
    | Some _, Some _ -> Some 1UL
    | _ -> None

/// Hand-fused triangle count from the `Graph.Impl.COO.TriangleCount` doc comment: never sorts.
let private triangleCountFused (g: Matrix.SparseMatrix<'a>) =
    let lower = Matrix.getLowerTriangle g

    Operations.mxmUnsorted tcAdd tcMult lower (Matrix.transposeUnsorted lower)
    |> unwrap
    |> fun c -> Matrix.maskUnsorted c lower Option.isSome
    |> unwrap
    |> Matrix.foldAssociativeUnsorted tcAdd None

[<Fact>]
let ``7V Triangle count fused`` () =
    Assert.Equal(Some 5UL, triangleCountFused sevenV)

// ============== Random graphs: COO agrees with the quad-tree ==============

/// Random graph on 1..12 vertices; `undirected` mirrors every edge.
let private randomEdges (n: byte) (edges: (byte * byte * byte) list) (undirected: bool) =
    let size = uint64 (int n % 12 + 1)

    let directed =
        edges
        |> List.map (fun (r, c, w) -> uint64 r % size, uint64 c % size, float (int w % 9 + 1))

    let all =
        if undirected then
            directed |> List.collect (fun (r, c, w) -> [ r, c, w; c, r, w ])
        else
            directed

    size, all |> List.distinctBy (fun (r, c, _) -> r, c)

let private qGraph size (edges: (uint64 * uint64 * 'a) list) =
    QMatrix.CoordinateList(
        size * 1UL<nrows>,
        size * 1UL<ncols>,
        edges |> List.map (fun (r, c, v) -> r * 1UL<rowindex>, c * 1UL<colindex>, v)
    )
    |> QMatrix.fromCoordinateList
    |> unwrap

let private qStart size s =
    QVector.CoordinateList(size * 1UL<dataLength>, [ s * 1UL<index>, 1UL ])
    |> QVector.fromCoordinateList
    |> unwrap

let private qDense size (vector: QVector.SparseVector<'a>) =
    List.init (int size) (fun i -> QVector.unsafeGet vector (uint64 i * 1UL<index>))

[<Property>]
let ``bfsLevel and bfsParent agree with quad-tree`` (n: byte) (edges: (byte * byte * byte) list) (s: byte) =
    let size, es = randomEdges n edges false
    let s = uint64 s % size
    let q, c = qGraph size es, graph size es

    (QBFS.bfsLevel q (qStart size s) |> unwrap |> qDense size) = (CBFS.bfsLevel c (start size s) |> unwrap |> dense size)
    && (QBFS.bfsParent q (qStart size s) |> unwrap |> qDense size) = (CBFS.bfsParent c (start size s)
                                                                      |> unwrap
                                                                      |> dense size)

[<Property>]
let ``sssp agrees with quad-tree`` (n: byte) (edges: (byte * byte * byte) list) (s: byte) =
    let size, es = randomEdges n edges false
    let s = uint64 s % size

    (QSSSP.sssp (qGraph size es) s |> unwrap |> qDense size) = (CSSSP.sssp (graph size es) s |> unwrap |> dense size)

[<Property>]
let ``triangleCount and its fused version agree with quad-tree`` (n: byte) (edges: (byte * byte * byte) list) =
    let size, es = randomEdges n edges true
    let es = es |> List.filter (fun (r, c, _) -> r <> c)
    let expected = QTC.triangleCount (qGraph size es) |> unwrap

    expected = (CTC.triangleCount (graph size es) |> unwrap)
    && expected = triangleCountFused (graph size es)

/// BFS with every operation replaced by its unsorted twin. The first-wins `op_add` of
/// `bfsParent` is not commutative, so parents may differ from the sorted run, but the
/// result must still be a valid BFS tree with the same levels.
let private unsortedBfsOps<'edge>
    : Graph.BFS.BfsOps<_, _, Vector.SparseVector<uint64>, Matrix.SparseMatrix<'edge>, _, _> =
    { CBFS.bfsOps with
        mapi = Vector.mapiUnsorted
        map2 = Vector.map2Unsorted
        mask = Vector.maskUnsorted
        vxmiValues = Operations.vxmi_valuesUnsorted }

[<Property>]
let ``bfsParent with unsorted ops yields a valid BFS tree`` (n: byte) (edges: (byte * byte * byte) list) (s: byte) =
    let size, es = randomEdges n edges false
    let s = uint64 s % size
    let g = graph size es
    let edgeSet = es |> List.map (fun (r, c, _) -> r, c) |> Set.ofList

    let levels = CBFS.bfsLevel g (start size s) |> unwrap |> dense size

    let unsortedLevels =
        Graph.BFS.bfsLevel unsortedBfsOps g (start size s)
        |> unwrap
        |> Vector.sort
        |> dense size

    let parents =
        Graph.BFS.bfsParent unsortedBfsOps g (start size s)
        |> unwrap
        |> Vector.sort
        |> dense size

    let validParent v =
        match parents.[v], levels.[v] with
        | None, None -> true
        | Some p, Some l when uint64 v = s -> p = s && l = 0UL
        | Some p, Some l -> edgeSet.Contains(p, uint64 v) && levels.[int p] = Some(l - 1UL)
        | _ -> false

    unsortedLevels = levels && List.forall validParent [ 0 .. int size - 1 ]
