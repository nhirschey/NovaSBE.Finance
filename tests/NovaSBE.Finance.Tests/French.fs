module NovaSBE.Finance.Tests.French

open Xunit
open FsUnit.Xunit
open FsUnitTyped

open NovaSBE.Finance.French

[<Fact>]
let ``Can do ff3 daily`` () =
    let ff3d = getFF3 Frequency.Daily
    ff3d.Length |> shouldBeGreaterThan 10

[<Fact>]
let ``Can do ff3 monthly`` () =
    let ff3m = getFF3 Frequency.Monthly
    ff3m.Length |> shouldBeGreaterThan 10

[<Fact>]
let ``Can do ff5 daily`` () =
    let ff5d = getFF5 Frequency.Daily
    ff5d.Length |> shouldBeGreaterThan 10

[<Fact>]
let ``Can do ff5 monthly`` () =
    let ff5m = getFF5 Frequency.Monthly
    ff5m.Length |> shouldBeGreaterThan 10

