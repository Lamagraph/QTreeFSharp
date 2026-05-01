namespace QuadTree.Benchmarks.AVLSet

open System.Threading.Tasks
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open QuadTree.AVLSet

[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
[<HtmlExporter>]
[<MemoryDiagnoser>]
[<ThreadingDiagnoser>]
[<HardwareCounters(HardwareCounter.CacheMisses, HardwareCounter.BranchMispredictions)>]
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
    val mutable public Threads: int

    [<DefaultValue>]
    val mutable public rndInt: int

    [<DefaultValue>]
    val mutable public setA: AVLTree<int>

    [<DefaultValue>]
    val mutable public setB: AVLTree<int>

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
        AVLSet.unionTraversal self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Union")>]
    member self.``Parallel union with threads``() =
        let opts = ParallelOptions()
        opts.MaxDegreeOfParallelism <- self.Threads

        AVLSet.parallelUnion opts self.setA self.setB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Intersection")>]
    member self.``Sequential intersection``() = AVLSet.intersection self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.``Intersection via tree traversal``() =
        AVLSet.intersectionTraversal self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.``Parallel intersection with threads``() =
        let opts = ParallelOptions()
        opts.MaxDegreeOfParallelism <- self.Threads

        AVLSet.parallelIntersection opts self.setA self.setB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Difference")>]
    member self.``Sequential difference``() = AVLSet.difference self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.``Difference via tree traversal``() =
        AVLSet.differenceTraversal self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.``Parallel difference with threads``() =
        let opts = ParallelOptions()
        opts.MaxDegreeOfParallelism <- self.Threads

        AVLSet.parallelDifference opts self.setA self.setB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Symmetrical Difference")>]
    member self.``Sequential symmetrical difference``() =
        AVLSet.symmDifference self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Symmetrical Difference")>]
    member self.``Symmetrical difference via tree traversal``() =
        AVLSet.symmDifferenceTraversal self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Symmetrical Difference")>]
    member self.``Parallel symmetrical difference with threads``() =
        let opts = ParallelOptions()
        opts.MaxDegreeOfParallelism <- self.Threads

        AVLSet.parallelSymmDifference opts self.setA self.setB
