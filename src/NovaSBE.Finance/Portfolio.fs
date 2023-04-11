module NovaSBE.Finance.Portfolio


open System

/// Misc
/// 

type SecurityId =
    | Ticker of string
    | Cusip of string
    | Bloomberg of string
    | Permno of int
    | Other of string

type InvestmentUniverse = 
    { FormationMonth : DateTime 
      Securities : SecurityId list }

type SecuritySignal = 
    { SecurityId : SecurityId
      Signal : float }

type SecuritiesWithSignals =
    { FormationMonth : DateTime 
      Signals : SecuritySignal list }

type PortfolioId = 
    | Named of string
    | Indexed of {| Name: string; Index: int |}
    override this.ToString() = 
        match this with
        | Named name -> name
        | Indexed p -> $"{p.Name}: {p.Index}"

type AssignedPortfolio =
    { PortfolioId : PortfolioId
      FormationMonth : DateTime 
      Signals : SecuritySignal list }


let assignSignalSort name n (xs: SecuritiesWithSignals) =
    xs.Signals
    |> List.sortBy(fun x -> x.Signal)
    |> List.splitInto n
    |> List.mapi(fun i ys -> 
        // because lists are 0-indexed and I want the minimum
        // portfolio index to be 1, I'm doing index = i+1.
        { PortfolioId = Indexed {| Name = name; Index=i+1 |}
          FormationMonth = xs.FormationMonth
          Signals = ys })

type Position =
    { SecurityId : SecurityId 
      Weight : float }

type Portfolio = 
    { PortfolioId: PortfolioId
      FormationMonth : DateTime
      Positions : Position list }

type PortfolioReturn =
    { PortfolioId: PortfolioId
      YearMonth : DateTime
      Return : float }

// This type alias defines the type of a function that has
// a tuple input of SecurityId * YearMonth and outputs
// a (SecurityId * float) Option. Think of it like
// using types to write documentation of the functions.
type GetsMarketCaps = SecurityId * DateTime -> (SecurityId * float) Option

// Defining this type alias makes it easier to read the type of the function that I want for
// marketCapGetter in the function that I have below. Otherwise it might look something like
// let giveValueWeights (marketCapGetter: (SecurityId * YearMonth -> (SecurityId * float) Option) ...
// which is the same thing but not as clear what we're trying to do.
let giveValueWeights (marketCapGetter: GetsMarketCaps) (x: AssignedPortfolio) =
    let mktCaps =
        x.Signals
        // List.choose throws away None results, so this means 
        // that if the market cap getter returns None
        // then we will not have that security in our portfolio.
        |> List.choose(fun signal -> marketCapGetter (signal.SecurityId, x.FormationMonth))

    let totMktCap = mktCaps |> List.sumBy snd

    let pos =
        mktCaps
        |> List.map(fun (id, mktCap) ->
            { SecurityId = id 
              Weight = mktCap / totMktCap })
    { PortfolioId = x.PortfolioId
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
        |> List.sumBy(fun pos -> 
            let (_id, ret) = returnGetter (pos.SecurityId, returnMonth)
            pos.Weight * ret)
    { PortfolioId = x.PortfolioId
      YearMonth = returnMonth
      Return = portRet }


/// This function takes a sample start and sample end
/// and returns a list with all months from start to end.
/// Don't worry about understanding what this function does.
/// The details are beyond the scope of the class, but if you're
/// curious it's a recursive function:
/// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/recursive-functions-the-rec-keyword
let getSampleMonths (sampleStart:DateTime, sampleEnd:DateTime) =
    let sampleStart = DateTime(sampleStart.Year, sampleStart.Month,1)
    let sampleEnd = DateTime(sampleEnd.Year, sampleEnd.Month,1)
    if sampleEnd <= sampleStart then failwith "sampleEnd should be after sampleStart"
    let rec loop (sampleEnd:DateTime) window = 
        match window with
        | [] -> failwith "Need a starting point"
        | lastMonth::_monthsBeforeThat ->
            if lastMonth < sampleEnd then 
                loop sampleEnd (lastMonth.AddMonths(1)::window)
            else window
    loop sampleEnd [sampleStart]
    |> List.rev

type SecurityReturn =
    { SecurityId: SecurityId
      Date: DateTime
      Return: float }
type Signal =
    { SecurityId: SecurityId 
      FormationDate: DateTime 
      Signal: float }
type Backtest(returns:seq<SecurityReturn>,signals:seq<Signal>) =
    let returns = returns |> Seq.toArray
    let signals = signals |> Seq.toArray
    let getReturn (id:SecurityId, date:DateTime) =
        returns
        |> Array.tryFind(fun x -> x.SecurityId = id && x.Date = date)
        |> Option.map(fun x -> (x.SecurityId, x.Return))
    let getSignal (id:SecurityId, date:DateTime) =
        signals
        |> Array.tryFind(fun x -> x.SecurityId = id && x.FormationDate = date)
        |> Option.map(fun x -> (x.SecurityId, x.Signal))
    let getMarketCap (id:SecurityId, date:DateTime) =
        None
    let getSampleMonths (sampleStart:DateTime, sampleEnd:DateTime) =
        getSampleMonths (sampleStart, sampleEnd)
    let assignSignalSort name n (xs: SecuritiesWithSignals) =
        assignSignalSort name n xs
    let giveValueWeights (x: AssignedPortfolio) =
        giveValueWeights getMarketCap x
    let getPortfolioReturn (x: Portfolio) =
        getPortfolioReturn getReturn x
    member this.GetSampleMonths (sampleStart:DateTime, sampleEnd:DateTime) =
        getSampleMonths (sampleStart, sampleEnd)
    member this.AssignSignalSort name n (xs: SecuritiesWithSignals) =
        assignSignalSort name n xs
    member this.GiveValueWeights (x: AssignedPortfolio) =
        giveValueWeights x
    member this.GetPortfolioReturn (x: Portfolio) =
        getPortfolioReturn x