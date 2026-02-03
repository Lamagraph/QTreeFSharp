module Tests

open System
open Xunit

open Common

[<Fact>]
let ``Power of two for 1`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 1UL = 1UL)

[<Fact>]
let ``Power of two for 2`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 2UL = 2UL)

[<Fact>]
let ``Power of two for 3`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 3UL = 4UL)

[<Fact>]
let ``Power of two for 12`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 12UL = 16UL)

[<Fact>]
let ``Power of two for 1025`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 1025UL = 2048UL)

[<Fact>]
let ``Power of two for 524290`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 524290UL = 1048576UL)

[<Fact>]
let ``Power of two for 1048576`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 1048576UL = 1048576UL)

[<Fact>]
let ``Power of two for 4611686018427388001`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 4611686018427388001UL = 9223372036854776000UL)
