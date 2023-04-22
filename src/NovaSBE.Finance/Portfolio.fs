module NovaSBE.Finance.Portfolio


open System

type SecurityId =
    | Ticker of string
    | Cusip of string
    | Bloomberg of string
    | Permno of int
    | Other of string

type InvestmentUniverse =
    { FormationMonth: DateTime
      Securities: SecurityId list }

type SecuritySignal =
    { SecurityId: SecurityId
      Signal: float }

type Position =
    { SecurityId: SecurityId
      Weight: float }

type Positions =
    { FormationMonth: DateTime
      Positions: list<Position> }

type Portfolio =
    { Name: string
      Index: int
      FormationMonth: DateTime
      Positions: Position list }

type PortfolioReturn =
    { Name: string
      Index: int
      Month: DateTime
      Return: float }

type BackTestResult =
    { Portfolios: Portfolio list
      Returns: PortfolioReturn list }

type SecuritiesWithSignals =
    { FormationMonth: DateTime
      Signals: SecuritySignal list }

    member this.weightedEqually() =
        { FormationMonth = this.FormationMonth
          Positions =
            let n = float this.Signals.Length

            this.Signals
            |> List.map (fun x ->
                { SecurityId = x.SecurityId
                  Weight = 1.0 / n }) }

type AssignedPortfolio =
    { Name: string
      Index: int
      FormationMonth: DateTime
      Signals: SecuritySignal list }


let assignSignalSort name n (xs: SecuritiesWithSignals) =
    xs.Signals
    |> Seq.sortBy (fun x -> x.Signal)
    |> Seq.splitInto n
    |> Seq.mapi (fun i ys ->
        // because lists are 0-indexed and I want the minimum
        // portfolio index to be 1, I'm doing index = i+1.
        { Name = name
          Index = i + 1 
          FormationMonth = xs.FormationMonth
          Signals = ys |> Array.toList })


/// This type alias defines the type of a function that has
/// a tuple input of SecurityId * YearMonth and outputs
/// a (SecurityId * float) Option. Think of it like
/// using types to write documentation of the functions.
type GetsMarketCaps = SecurityId * DateTime -> (SecurityId * float) Option

/// Defining this type alias makes it easier to read the type of the function that I want for
/// marketCapGetter in the function that I have below. Otherwise it might look something like
/// let giveValueWeights (marketCapGetter: (SecurityId * YearMonth -> (SecurityId * float) Option) ...
/// which is the same thing but not as clear what we're trying to do.
let giveValueWeights (marketCapGetter: GetsMarketCaps) (x: AssignedPortfolio) =
    let mktCaps =
        x.Signals
        // List.choose throws away None results, so this means
        // that if the market cap getter returns None
        // then we will not have that security in our portfolio.
        |> List.choose (fun signal -> marketCapGetter (signal.SecurityId, x.FormationMonth))

    let totMktCap = mktCaps |> List.sumBy snd

    let pos =
        mktCaps
        |> List.map (fun (id, mktCap) ->
            { SecurityId = id
              Weight = mktCap / totMktCap })

    { Name = x.Name
      Index = x.Index
      FormationMonth = x.FormationMonth
      Positions = pos }

let giveEqualWeights (x: AssignedPortfolio) =
    let n = x.Signals.Length

    let pos =
        x.Signals
        |> List.map (fun signal ->
            { SecurityId = signal.SecurityId
              Weight = 1.0 / float n })

    { Name = x.Name
      Index = x.Index
      FormationMonth = x.FormationMonth
      Positions = pos }

// This type alias defines the type of a function that has
// a tuple input of SecurityId * YearMonth and outputs
// a (SecurityId * float). Think of it like
// using types to write documentation of the functions.
type GetsReturn = SecurityId * DateTime -> (SecurityId * float)

// Defining this type alias makes it easier to read the type of the function that I want for
// marketCapGetter in the function that I have below. Otherwise it might look something like
// let giveValueWeights (marketCapGetter: (SecurityId * YearMonth -> (SecurityId * float) Option) ...
// which is the same thing but not as clear what we're trying to do.
let getPortfolioReturn (returnGetter: GetsReturn) (x: Portfolio) =
    let returnMonth = x.FormationMonth.AddMonths(1)

    let portRet =
        x.Positions
        |> List.sumBy (fun pos ->
            let (_id, ret) = returnGetter (pos.SecurityId, returnMonth)
            pos.Weight * ret)

    { Name = x.Name
      Index = x.Index
      Month = returnMonth
      Return = portRet }


/// This function takes a sample start and sample end
/// and returns a list with all months from start to end.
/// Don't worry about understanding what this function does.
/// The details are beyond the scope of the class, but if you're
/// curious it's a recursive function:
/// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/recursive-functions-the-rec-keyword
let getSampleMonths (sampleStart: DateTime, sampleEnd: DateTime) =
    let sampleStart = DateTime(sampleStart.Year, sampleStart.Month, 1)
    let sampleEnd = DateTime(sampleEnd.Year, sampleEnd.Month, 1)

    if sampleEnd <= sampleStart then
        failwith "sampleEnd should be after sampleStart"

    let rec loop (sampleEnd: DateTime) window =
        match window with
        | [] -> failwith "Need a starting point"
        | lastMonth :: _monthsBeforeThat ->
            if lastMonth < sampleEnd then
                loop sampleEnd (lastMonth.AddMonths(1) :: window)
            else
                window

    loop sampleEnd [ sampleStart ] |> List.rev

type SecurityReturn =
    { SecurityId: SecurityId
      Date: DateTime
      Return: float }

type Signal =
    { SecurityId: SecurityId
      FormationDate: DateTime
      Signal: float }

type WeightVariable =
    { SecurityId: SecurityId
      FormationDate: DateTime
      Value: float }

type SignalStrength =
    | Zscore
    | Rank

/// <summary>Constructs a backtest of a trading strategy based on signals.</summary>
/// <param name="returns">A sequence of security returns.</param>
/// <param name="signals">A sequence of signals for each security and formation date.</param>
/// <param name="nPortfolios">The number of cross-sectionally sorted portfolios to create.</param>
/// <param name="name">The name of the strategy.</param>
/// <param name="inParallel">Whether to run the backtest in parallel.</param>
/// <returns>The portfolio positions each formation date and the portfolio returns.</returns>
type Backtest(returns: seq<SecurityReturn>, signals: seq<Signal>, nPortfolios: int, ?name: string, ?inParallel: bool) =
    let name = defaultArg name "Signal Sort"
    let inParallel = defaultArg inParallel true

    let returnMap =
        returns |> Seq.map (fun x -> (x.SecurityId, x.Date), x.Return) |> dict

    let getReturns (securityId, date) =
        match returnMap.TryGetValue((securityId, date)) with
        | true, ret -> securityId, ret
        | _ -> securityId, 0.0

    let assigned =
        signals
        |> Seq.groupBy (fun x -> x.FormationDate)
        |> Seq.toArray
        |> (if inParallel then Array.Parallel.collect else Array.collect) (fun (date, xs) ->
            { SecuritiesWithSignals.FormationMonth = date
              Signals =
                xs
                |> Seq.toList
                |> List.map (fun x ->
                    { SecurityId = x.SecurityId
                      Signal = x.Signal }) }
            |> assignSignalSort name nPortfolios
            |> Seq.toArray)

    /// <summary>Value weighted strategy</summary>
    /// <param name="marketCaps">Market cap data</param>
    member this.strategyValueWeighted(marketCaps) =
        let marketCaps =
            marketCaps
            |> Seq.map (fun x -> (x.SecurityId, x.FormationDate), x.Value)
            |> dict

        let marketCapGetter (securityId, date) =
            match marketCaps.TryGetValue((securityId, date)) with
            | true, marketCap -> Some(securityId, marketCap)
            | _ -> None

        let portfolios =
            assigned
            |> Seq.toArray
            |> (if inParallel then Array.Parallel.map else Array.map) (giveValueWeights marketCapGetter)
            
        let returns =
            portfolios
            |> (if inParallel then Array.Parallel.map else Array.map) (getPortfolioReturn getReturns)
        
        { Portfolios = portfolios |> Array.toList
          Returns = returns |> Array.toList }

    /// <summary>Equal weighted strategy</summary>
    member this.strategyEqualWeighted() =
        let portfolios =
            assigned
            |> Seq.toArray
            |> (if inParallel then Array.Parallel.map else Array.map) giveEqualWeights
        let returns =
            portfolios
            |> (if inParallel then Array.Parallel.map else Array.map) (getPortfolioReturn getReturns)

        { Portfolios = portfolios |> Array.toList
          Returns = returns |> Array.toList }