namespace QuadTree.Benchmarks.ReduceComparison

open System
open System.IO
open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils

[<Config(typeof<MyConfig>)>]
[<MemoryDiagnoser>]
type Benchmark() =

    let add x y =
        match x, y with
        | Some a, Some b -> Some(a + b)
        | Some a, None
        | None, Some a -> Some a
        | _ -> None

    [<Params("bcsstk01",
             "bcsstk02",
             "bcsstk03",
             "bcsstk04",
             "bcsstk05",
             "bcsstk06",
             "bcsstk07",
             "bcsstk08",
             "bcsstk09",
             "bcsstk10",
             "bcsstk11",
             "bcsstk12",
             "bcsstk13",
             "bcsstk14",
             "bcsstk15",
             "bcsstk16",
             "bcsstk17",
             "bcsstk18",
             "bcsstk29",
             "bcsstk30",
             "bcsstk31",
             "cavity01",
             "cavity05",
             "cavity10",
             "mesh2e1",
             "mesh3em5",
             "pwt",
             "shuttle_eddy",
             "tandem_vtx",
             "email-Eu-core")>]
    member val MatrixName = "" with get, set

    member val Matrix = Unchecked.defaultof<Matrix.SparseMatrix<double>> with get, set
    member val Size = 0 with get, set
    member val Density = 0.0 with get, set
    member val IsSymmetric = false with get, set

    member private this.CheckSymmetric(m: Matrix.SparseMatrix<double>) =
        let coo = Matrix.toCoordinateList m
        let dict = System.Collections.Generic.Dictionary<string, double>()

        for (i, j, v) in coo.list do
            let key = $"{uint64 i},{uint64 j}"
            dict.[key] <- v

        let mutable sym = true

        for (i, j, v) in coo.list do
            let key = $"{uint64 j},{uint64 i}"

            match dict.TryGetValue(key) with
            | true, v2 when v = v2 -> ()
            | _ -> sym <- false

        sym

    [<GlobalSetup>]
    member this.Setup() =
        let rec findProjectRoot (dir: string) =
            if Directory.Exists(Path.Combine(dir, "data")) then
                dir
            else
                let parent = Directory.GetParent(dir)

                if parent = null then
                    failwith "Не найден корень проекта (папка data)"
                else
                    findProjectRoot parent.FullName

        let projectRoot = findProjectRoot __SOURCE_DIRECTORY__

        let path =
            Path.Combine(projectRoot, "data", this.MatrixName, $"{this.MatrixName}.mtx")

        if not (File.Exists path) then
            failwithf "Файл не найден: %s\nИщем в: %s" path projectRoot

        match QuadTree.Benchmarks.Utils.readMtx path false with
        | Ok m ->
            this.Matrix <- m
            this.Size <- int m.nrows
            this.Density <- float m.nvals / (float m.nrows * float m.ncols)
            this.IsSymmetric <- this.CheckSymmetric(m)
        | Error msg -> failwithf "Не удалось загрузить %s: %s" this.MatrixName msg

    [<Benchmark>]
    member this.ReduceCols_Original() = Matrix.reduceCols add this.Matrix

    [<Benchmark>]
    member this.ReduceCols_ViaTranspose() =
        let transposed = Matrix.transpose this.Matrix
        Matrix.reduceRows add transposed
