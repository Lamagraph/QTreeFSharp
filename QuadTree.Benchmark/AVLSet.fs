namespace QuadTree.Benchmarks.AVLSet

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open QuadTree.AVLSet
open QuadTree.AVLSet.Parallel
open System.Collections.Generic

[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
[<HtmlExporter>]
[<MemoryDiagnoser>]
type SingleOpsBenchmark() =
    let rnd = System.Random(1234561)

    [<Params(100, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public A: int

    [<DefaultValue>]
    val mutable public rndInt: int

    [<DefaultValue>]
    val mutable public setA: AVLSet<int>

    [<GlobalSetup>]
    member self.Setup() =
        self.rndInt <- rnd.Next(self.A + 1, self.A + 1000)

        let dataA = Array.init self.A (fun _ -> rnd.Next())

        self.setA <-
            dataA
            |> Array.fold
                (fun (set: AVLSet<int>) v ->
                    match AVLSet.add v set with
                    | Ok nextSet -> nextSet
                    | Error err -> failwithf "Benchmark setup failed: %A" err)
                AVLSet.empty

    [<Benchmark>]
    [<BenchmarkCategory("Adding")>]
    member self.AddingOneElement() = AVLSet.add self.rndInt self.setA

    [<Benchmark>]
    [<BenchmarkCategory("Deleting")>]
    member self.DeletingOneElement() = AVLSet.delete self.rndInt self.setA


[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
[<HtmlExporter>]
[<MemoryDiagnoser>]
type TraversalSetsBenchmark() =
    let rnd = System.Random(1234561)

    [<Params(100, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public A: int

    [<Params(100, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public B: int

    [<DefaultValue>]
    val mutable public setA: AVLSet<int>

    [<DefaultValue>]
    val mutable public setB: AVLSet<int>

    [<GlobalSetup>]
    member self.Setup() =
        let dataA = Array.init self.A (fun _ -> rnd.Next())

        let dataB = Array.init self.B (fun _ -> rnd.Next())

        self.setA <-
            dataA
            |> Array.fold
                (fun (set: AVLSet<int>) v ->
                    match AVLSet.add v set with
                    | Ok nextSet -> nextSet
                    | Error err -> failwithf "Benchmark setup failed: %A" err)
                AVLSet.empty

        self.setB <-
            dataB
            |> Array.fold
                (fun (set: AVLSet<int>) v ->
                    match AVLSet.add v set with
                    | Ok nextSet -> nextSet
                    | Error err -> failwithf "Benchmark setup failed: %A" err)
                AVLSet.empty

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Union")>]
    member self.SequentialUnion() = AVLSet.union self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Union")>]
    member self.UnionViaTreeTraversal() =
        AVLSet.Traversal.union self.setA self.setB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Intersection")>]
    member self.SequentialIntersection() = AVLSet.intersection self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.IntersectionViaTreeTraversal() =
        AVLSet.Traversal.intersection self.setA self.setB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Difference")>]
    member self.SequentialDifference() = AVLSet.difference self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.DifferenceViaTreeTraversal() =
        AVLSet.Traversal.difference self.setA self.setB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Symmetrical Difference")>]
    member self.SequentialSymmetricalDifference() =
        AVLSet.symmDifference self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Symmetrical Difference")>]
    member self.SymmetricalDifferenceViaTreeTraversal() =
        AVLSet.Traversal.symmDifference self.setA self.setB


[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
[<HtmlExporter>]
[<MemoryDiagnoser>]
[<ThreadingDiagnoser>]
type ParallelSetsBenchmark() =
    let rnd = System.Random(1234561)

    [<Params(1000, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public A: int

    [<Params(100, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public B: int

    [<Params(1, 2, 4)>]
    [<DefaultValue>]
    val mutable public threads: int

    [<DefaultValue>]
    val mutable public setA: AVLSet<int>

    [<DefaultValue>]
    val mutable public setB: AVLSet<int>

    [<GlobalSetup>]
    member self.Setup() =
        let dataA = Array.init self.A (fun _ -> rnd.Next())

        let dataB = Array.init self.B (fun _ -> rnd.Next())

        self.setA <-
            dataA
            |> Array.fold
                (fun set v ->
                    match AVLSet.add v set with
                    | Ok s -> s
                    | Error e -> failwithf "%A" e)
                AVLSet.empty

        self.setB <-
            dataB
            |> Array.fold
                (fun set v ->
                    match AVLSet.add v set with
                    | Ok s -> s
                    | Error e -> failwithf "%A" e)
                AVLSet.empty


    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Union")>]
    member self.SequentialUnion() = AVLSet.union self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Union")>]
    member self.ParallelUnionWithThreads() =
        ParallelAVLSet.union (Some self.threads) 10 self.setA self.setB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Intersection")>]
    member self.SequentialIntersection() = AVLSet.intersection self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.ParallelIntersectionWithThreads() =
        ParallelAVLSet.intersection (Some self.threads) 10 self.setA self.setB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Difference")>]
    member self.SequentialDifference() = AVLSet.difference self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.ParallelDifferenceWithThreads() =
        ParallelAVLSet.difference (Some self.threads) 10 self.setA self.setB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Symmetrical Difference")>]
    member self.SequentialSymmetricalDifference() =
        AVLSet.symmDifference self.setA self.setB

    [<Benchmark>]
    [<BenchmarkCategory("Symmetrical Difference")>]
    member self.ParallelSymmetricalDifferenceWithThreads() =
        ParallelAVLSet.symmDifference (Some self.threads) 10 self.setA self.setB

[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
[<HtmlExporter>]
[<MemoryDiagnoser>]
[<ThreadingDiagnoser>]
type FSSetsBenchmark() =
    let rnd = System.Random(1234561)

    [<Params(1000, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public A: int

    [<Params(100, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public B: int

    [<Params(1, 2, 4)>]
    [<DefaultValue>]
    val mutable public threads: int

    [<DefaultValue>]
    val mutable public AVLSetA: AVLSet<int>

    [<DefaultValue>]
    val mutable public AVLSetB: AVLSet<int>

    [<DefaultValue>]
    val mutable public SetA: Set<int>

    [<DefaultValue>]
    val mutable public SetB: Set<int>

    [<DefaultValue>]
    val mutable public HashSetA: HashSet<int>

    [<DefaultValue>]
    val mutable public HashSetB: HashSet<int>

    [<GlobalSetup>]
    member self.Setup() =
        let dataA = Array.init self.A (fun _ -> rnd.Next())

        let dataB = Array.init self.B (fun _ -> rnd.Next())

        self.AVLSetA <-
            dataA
            |> Array.fold
                (fun set v ->
                    match AVLSet.add v set with
                    | Ok s -> s
                    | Error e -> failwithf "%A" e)
                AVLSet.empty

        self.AVLSetB <-
            dataB
            |> Array.fold
                (fun set v ->
                    match AVLSet.add v set with
                    | Ok s -> s
                    | Error e -> failwithf "%A" e)
                AVLSet.empty

        self.SetA <- dataA |> Array.fold (fun set v -> Set.add v set) Set.empty

        self.SetB <- dataB |> Array.fold (fun set v -> Set.add v set) Set.empty

        self.HashSetA <- HashSet<int>()
        dataA |> Array.iter (fun v -> self.HashSetA.Add(v) |> ignore)

        self.HashSetB <- HashSet<int>()
        dataB |> Array.iter (fun v -> self.HashSetA.Add(v) |> ignore)

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Union")>]
    member self.SequentialUnionAVL() = AVLSet.union self.AVLSetA self.AVLSetB

    [<Benchmark>]
    [<BenchmarkCategory("Union")>]
    member self.ParallelUnionWithThreadsAVL() =
        ParallelAVLSet.union (Some self.threads) 10 self.AVLSetA self.AVLSetB

    [<Benchmark>]
    [<BenchmarkCategory("Union")>]
    member self.UnionViaTreeTraversalAVL() =
        AVLSet.Traversal.union self.AVLSetA self.AVLSetB

    [<Benchmark>]
    [<BenchmarkCategory("Union")>]
    member self.UnionFS() = Set.union self.SetA self.SetB

    [<Benchmark>]
    [<BenchmarkCategory("Union")>]
    member self.UnionHashFS() = self.HashSetA.UnionWith(self.HashSetB)

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Intersection")>]
    member self.SequentialIntersectionAVL() =
        AVLSet.intersection self.AVLSetA self.AVLSetB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.ParallelIntersectionWithThreadsAVL() =
        ParallelAVLSet.intersection (Some self.threads) 10 self.AVLSetA self.AVLSetB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.IntersectionViaTreeTraversalAVL() =
        AVLSet.Traversal.intersection self.AVLSetA self.AVLSetB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.IntersectionFS() = Set.intersect self.SetA self.SetB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.IntersectionHashFS() =
        self.HashSetA.IntersectWith(self.HashSetB)

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Difference")>]
    member self.SequentialDifferenceAVL() =
        AVLSet.difference self.AVLSetA self.AVLSetB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.ParallelDifferenceWithThreadsAVL() =
        ParallelAVLSet.difference (Some self.threads) 10 self.AVLSetA self.AVLSetB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.DifferenceViaTreeTraversalAVL() =
        AVLSet.Traversal.difference self.AVLSetA self.AVLSetB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.DifferenceFS() = Set.difference self.SetA self.SetB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.DifferenceHashFS() = self.HashSetA.ExceptWith(self.HashSetB)
