module NovaSBE.Finance.Portfolio2

open System

let stddev xs =
    let mu = xs |> Seq.average
    xs 
    |> Seq.map (fun x -> (x - mu) ** 2.0) 
    |> Seq.average 
    |> sqrt

type Signal = { SecurityId : string; Signal : float }
type MarketCap = { SecurityId: string; MktCap: float}


type Weighting = { SecurityId: string; Value: float}

type Position = { SecurityId : string; Weight : float }

type Return = { Id: string; DateTime: DateTime; Return: float }

type SignalStrength =
    | Zscore
    | Rank

/// <summary>Scales position weights to be $1 long and $1 short.</summary>
let dollarLongShort (ws: Weighting seq) =
    let posSum = ws |> Seq.sumBy (fun x -> if x.Value > 0 then x.Value else 0.0)
    let negSum = ws |> Seq.sumBy (fun x -> if x.Value < 0 then x.Value else 0.0)
    ws
    |> Seq.map (fun x ->
        let w = if x.Value > 0 then x.Value / posSum else -x.Value / negSum
        { SecurityId = x.SecurityId; Weight = w })
(*
let signalWeights strength signals =
    match strength with
    | Zscore -> 
        let s = signals |> Seq.map (fun x -> x.Signal)
        let mu = s |> Seq.average
        let sigma = s |> stddev
        signals
        |> Seq.map (fun x -> { SecurityId = x.SecurityId; Weight = (x.Signal - mu) / sigma })
        |> dollarLongShort
        |> Seq.toArray
    | Rank ->
        let n = Seq.length signals
        signals
        |> Seq.sortBy (fun x -> x.Signal)
        |> Seq.mapi (fun i x -> 
            { SecurityId = x.SecurityId; Weight = (float i - 0.5) / (float n) })
        |> dollarLongShort
        |> Seq.toArray
*)
let rankWeighting weightings =
    let n = Seq.length weightings
    weightings
    |> Seq.sortBy (fun x -> x.Value)
    |> Seq.mapi (fun i x -> 
        { SecurityId = x.SecurityId; Value = (float i - 0.5) / (float n) })

let zscoreWeighting weightings =
    let s = weightings |> Seq.map (fun x -> x.Value)
    let mu = s |> Seq.average
    let sigma = s |> stddev
    weightings
    |> Seq.map (fun x -> { SecurityId = x.SecurityId; Value = (x.Value - mu) / sigma })

type Portfolio(positions: Position seq) =
    let positions' = positions |> Seq.toArray
    static member equalWeight(securities: string seq) =
        let n = Seq.length securities |> float
        securities
        |> Seq.map (fun x -> { SecurityId = x; Weight = 1.0 / n })
        |> Portfolio
    static member valueWeight(marketCaps: MarketCap seq) =
        marketCaps
        |> Seq.map (fun x -> { SecurityId = x.SecurityId; Value = x.MktCap })
        |> dollarLongShort
        |> Portfolio
    
    member this.positions = positions'
    member this.securities = positions' |> Array.map (fun x -> x.SecurityId) |> Array.distinct

    member this.Return(returns: Return seq) =
        let dt = 
            try 
                returns |> Seq.map (fun x -> x.DateTime) |> Seq.distinct |> Seq.exactlyOne
            with 
            | _ -> failwith "Returns must be for a single date."
        let returns = returns |> Seq.map (fun x -> x.Id, x.Return) |> dict
        let positionReturn position =
            if returns.ContainsKey position.SecurityId then 
                position.Weight * returns[position.SecurityId]
            else 
                failwith $"No return given for Security {position.SecurityId}."
        let ret = positions |> Seq.sumBy positionReturn
        { Id = "Portfolio"; DateTime = dt; Return = ret }
        