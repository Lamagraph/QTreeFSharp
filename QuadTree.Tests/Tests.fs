module Tests

open System
open Xunit

open Common

[<Fact>]
let ``Power of two for 1`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 1UL = Ok 1UL)

[<Fact>]
let ``Power of two for 2`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 2UL = Ok 2UL)

[<Fact>]
let ``Power of two for 3`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 3UL = Ok 4UL)

[<Fact>]
let ``Power of two for 12`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 12UL = Ok 16UL)

[<Fact>]
let ``Power of two for 1025`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 1025UL = Ok 2048UL)

[<Fact>]
let ``Power of two for 524290`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 524290UL = Ok 1048576UL)

[<Fact>]
let ``Power of two for 1048576`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 1048576UL = Ok 1048576UL)

[<Fact>]
let ``Power of two for 4611686018427388001`` () =
    Assert.True(Common.getNearestUpperPowerOfTwo 4611686018427388001UL = Ok 9223372036854776000UL)

[<Fact>]
let ``Power of two for too large argument`` () =
    Assert.Equal(
        Error(Common.ArgumentTooLarge 9223372036854776000UL),
        Common.getNearestUpperPowerOfTwo 9223372036854776001UL
    )
