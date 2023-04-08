module NovaSBE.Finance.VolatilityManaged

open System
open Quotes.YahooFinance
open FSharp.Stats

type Vol =
    | Realized of int
    | ExponentialWeightedMovingAverage of float

type RealizedVol<'Index when 'Index: comparison>(data: seq<'Index * float>,?width: int) =
    let width = defaultArg width 22
    member _.Vols() =
        data
        |> Seq.windowed width
        |> Seq.map (fun xs ->
            let (lastIndex, _) = Seq.last xs
            lastIndex, xs |> Array.map snd |> stDev)

let realizedVol (data: seq<'Index * float>) (width: int) =
    data
    |> Seq.windowed width
    |> Seq.map (fun xs ->
        let (lastIndex, _) = Seq.last xs
        lastIndex, xs |> Array.map snd |> stDev)

let ewmaVol (data: seq<'Index * float>) (lambda: float) =
    if lambda <= 0 || lambda > 1.0 then failwith $"Lambda is {lambda} but it should be between 0.0 and 1.0"

/// Function to construct a volatility managed portfolio.
type VolManaged<'Index when 'Index: comparison>(data: seq<'Index * float>, constant: float, ?width: int) =
    let width = defaultArg width 22

    let strategy =
        data
        |> Seq.windowed (width + 1)
        |> Seq.map (fun window ->
            let expectedVol = window |> Seq.take width |> Seq.map snd |> stDev
            let (index, ret) = window |> Seq.last
            index, ret * (constant / expectedVol ** 2.0))
        |> Seq.toArray

    member this.Returns = strategy

    member this.CumulativeReturns =
        let mutable cr = 0.0

        [| for (dt, ret) in strategy do
               cr <- (1.0 + cr) * (1.0 + ret) - 1.0
               dt, cr |]


