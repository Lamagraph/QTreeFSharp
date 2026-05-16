namespace QuadTree.Benchmarks.AVLSet

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open QuadTree.AVLSet
open QuadTree.AVLSet.Parallel

[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
[<HtmlExporter>]
[<MemoryDiagnoser>]
[<ThreadingDiagnoser>]
type Benchmark() =
    let rnd = System.Random(1234561)

    [<Params(100, 10000, 1000000)>]
    [<DefaultValue>]
    val mutable public A: int

    [<Params(100, 1000, 100000)>]
    [<DefaultValue>]
    val mutable public B: int

    [<Params("Random", "Sorted")>]
    [<DefaultValue>]
    val mutable public DataTypeA: string

    [<Params(1, 2, 4, 8)>]
    [<DefaultValue>]
    val mutable public threads: int

    [<DefaultValue>]
    val mutable public rndInt: int

    [<DefaultValue>]
    val mutable public setA: AVLSet<int>

    [<DefaultValue>]
    val mutable public setB: AVLSet<int>

    [<GlobalSetup>]
    member self.Setup() =
        self.rndInt <- rnd.Next(self.A + 1, self.A + 1000)

        let dataA =
            match self.DataTypeA with
            | "Random" -> Array.init self.A (fun _ -> rnd.Next())
            | _ -> [| 1 .. self.A |]

        let dataB = Array.init self.B (fun _ -> rnd.Next())

        self.setA <- dataA |> Array.fold (fun s v -> AVLSet.add v s) AVLSet.empty
        self.setB <- dataB |> Array.fold (fun s v -> AVLSet.add v s) AVLSet.empty

    [<Benchmark>]
    [<BenchmarkCategory("Adding")>]
    member self.``Adding one element``() = AVLSet.add self.rndInt self.setA

    [<Benchmark>]
    [<BenchmarkCategory("Deleting")>]
    member self.``Deleting one element``() = AVLSet.delete self.rndInt self.setA

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Union")>]
    member self.``Sequential union``() = AVLSet.union self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Union")>]
    member self.``Union via tree traversal``() =
        AVLSet.Traversal.union self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Union")>]
    member self.``Parallel union with threads``() =
        ParallelAVLSet.union (Some self.threads) self.setA self.setB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Intersection")>]
    member self.``Sequential intersection``() = AVLSet.intersection self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.``Intersection via tree traversal``() =
        AVLSet.Traversal.intersection self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.``Parallel intersection with threads``() =
        ParallelAVLSet.intersection (Some self.threads) self.setA self.setB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Difference")>]
    member self.``Sequential difference``() = AVLSet.difference self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.``Difference via tree traversal``() =
        AVLSet.Traversal.difference self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.``Parallel difference with threads``() =
        ParallelAVLSet.difference (Some self.threads) self.setA self.setB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Symmetrical Difference")>]
    member self.``Sequential symmetric difference``() =
        AVLSet.symmDifference self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Symmetrical Difference")>]
    member self.``Symmetric difference via tree traversal``() =
        AVLSet.Traversal.symmDifference self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Symmetrical Difference")>]
    member self.``Parallel symmetric difference with threads``() =
        ParallelAVLSet.symmDifference (Some self.threads) self.setA self.setB
