namespace QuadTree.Benchmarks.RedBlackSet

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open QuadTree.RBSet.RBSet
open QuadTree.RBSet
open System.Collections.Generic

[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
[<HtmlExporter>]
[<MemoryDiagnoser>]
type BatchOpsBenchmark() =
    let rnd = System.Random(1234561)

    [<Params(100, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public A: int

    [<Params(10, 100, 1000)>]
    [<DefaultValue>]
    val mutable public N: int

    [<DefaultValue>]
    val mutable public data: int[]

    [<DefaultValue>]
    val mutable public toInsert: int[]

    [<DefaultValue>]
    val mutable public existingToDelete: int[]

    [<DefaultValue>]
    val mutable public missingToDelete: int[]

    [<DefaultValue>]
    val mutable public rndInt: int

    [<DefaultValue>]
    val mutable public setA: RBSet<int>

    [<DefaultValue>]
    val mutable public fsSet: Set<int>

    [<DefaultValue>]
    val mutable public initialRB: RBSet<int>

    [<DefaultValue>]
    val mutable public initialFS: Set<int>

    [<GlobalSetup>]
    member self.Setup() =
        self.data <- Array.init self.A (fun _ -> rnd.Next())

        self.initialRB <-
            self.data
            |> Array.fold
                (fun (set: RBSet<int>) v ->
                    match RBSet.add v set with
                    | Ok nextSet -> nextSet
                    | Error err -> failwithf "Setup failed: %A" err)
                RBSet.empty

        self.initialFS <- self.data |> Array.fold (fun s v -> Set.add v s) Set.empty

        let maxData = if self.data.Length > 0 then Array.max self.data else 0
        let minData = if self.data.Length > 0 then Array.min self.data else 0

        let insertCount = min self.N self.A
        self.toInsert <- Array.init insertCount (fun i -> maxData + 1000 + i)

        let uniqueExisting =
            self.data |> Array.distinct |> Array.truncate (min self.N self.A)

        let shuffleRnd = System.Random(42)
        let shuffled = Array.copy uniqueExisting

        for i in shuffled.Length - 1 .. -1 .. 1 do
            let j = shuffleRnd.Next(i + 1)
            let tmp = shuffled.[i]
            shuffled.[i] <- shuffled.[j]
            shuffled.[j] <- tmp

        self.existingToDelete <- shuffled

        let missingCount = min self.N self.A
        self.missingToDelete <- Array.init missingCount (fun i -> minData - 1000 - i)

        self.setA <- self.initialRB
        self.fsSet <- self.initialFS

    [<IterationSetup>]
    member self.IterationSetup() =
        self.setA <- self.initialRB
        self.fsSet <- self.initialFS

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("InsertBatch")>]
    member self.InsertBatchRB() =
        self.toInsert
        |> Array.fold
            (fun s v ->
                match RBSet.add v s with
                | Ok s' -> s'
                | Error _ -> s)
            self.setA

    [<Benchmark>]
    [<BenchmarkCategory("InsertBatch")>]
    member self.InsertBatchFS() =
        self.toInsert |> Array.fold (fun s v -> Set.add v s) self.fsSet

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("DeleteExistingBatch")>]
    member self.DeleteExistingBatchRB() =
        self.existingToDelete
        |> Array.fold
            (fun s v ->
                match RBSet.delete v s with
                | Ok s' -> s'
                | Error _ -> s)
            self.setA

    [<Benchmark>]
    [<BenchmarkCategory("DeleteExistingBatch")>]
    member self.DeleteExistingBatchFS() =
        self.existingToDelete |> Array.fold (fun s v -> Set.remove v s) self.fsSet

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("DeleteMissingBatch")>]
    member self.DeleteMissingBatchRB() =
        self.missingToDelete
        |> Array.fold
            (fun s v ->
                match RBSet.delete v s with
                | Ok s' -> s'
                | Error _ -> s)
            self.setA

    [<Benchmark>]
    [<BenchmarkCategory("DeleteMissingBatch")>]
    member self.DeleteMissingBatchFS() =
        self.missingToDelete |> Array.fold (fun s v -> Set.remove v s) self.fsSet


[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
[<HtmlExporter>]
[<MemoryDiagnoser>]
type SetsBenchmark() =
    let rnd = System.Random(1234561)

    [<Params(1000, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public A: int

    [<Params(100, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public B: int

    [<DefaultValue>]
    val mutable public RedBlackSetA: RBSet<int>

    [<DefaultValue>]
    val mutable public RedBlackSetB: RBSet<int>

    [<DefaultValue>]
    val mutable public SetA: Set<int>

    [<DefaultValue>]
    val mutable public SetB: Set<int>

    [<GlobalSetup>]
    member self.Setup() =
        let smaller = min self.A self.B
        let commonCount = int (float smaller * 0.25)

        let common = Array.init commonCount (fun _ -> rnd.Next())

        let uniqueACount = self.A - commonCount
        let uniqueBCount = self.B - commonCount

        let uniqueA = Array.init uniqueACount (fun _ -> rnd.Next())

        let uniqueB = Array.init uniqueBCount (fun _ -> rnd.Next())

        let dataA = Array.append common uniqueA
        let dataB = Array.append common uniqueB

        self.RedBlackSetA <-
            dataA
            |> Array.fold
                (fun set v ->
                    match RBSet.add v set with
                    | Ok s -> s
                    | Error e -> failwithf "%A" e)
                RBSet.empty

        self.RedBlackSetB <-
            dataB
            |> Array.fold
                (fun set v ->
                    match RBSet.add v set with
                    | Ok s -> s
                    | Error e -> failwithf "%A" e)
                RBSet.empty

        self.SetA <- dataA |> Array.fold (fun set v -> Set.add v set) Set.empty

        self.SetB <- dataB |> Array.fold (fun set v -> Set.add v set) Set.empty

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Union")>]
    member self.UnionRB() =
        RBSet.union self.RedBlackSetA self.RedBlackSetB

    [<Benchmark>]
    [<BenchmarkCategory("Union")>]
    member self.UnionFS() = Set.union self.SetA self.SetB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Intersection")>]
    member self.IntersectionRB() =
        RBSet.intersection self.RedBlackSetA self.RedBlackSetB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.IntersectionFS() = Set.intersect self.SetA self.SetB

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Difference")>]
    member self.DifferenceRB() =
        RBSet.difference self.RedBlackSetA self.RedBlackSetB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.DifferenceFS() = Set.difference self.SetA self.SetB
