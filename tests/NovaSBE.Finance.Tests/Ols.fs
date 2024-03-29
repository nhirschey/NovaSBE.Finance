#if INTERACTIVE
#r "../../src/NovaSBE.Finance/bin/Debug/net6.0/NovaSBE.Finance.dll"
#r "nuget: FsUnit.xUnit"
#r "nuget: FSharp.Data"
#r "nuget: FSharp.Stats"
#r "nuget: DiffSharp-lite"
#else
module NovaSBE.Finance.Tests.Ols
#endif

open System
open System.IO
open Xunit
open FsUnit.Xunit
open FsUnitTyped

open NovaSBE.Finance.Ols
open FSharp.Data

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let testDataDir = "TestData"
let testFiles = [ "HistData/Guerry.csv"; "datasets/iris.csv" ]

if not (Directory.Exists testDataDir) then
    Directory.CreateDirectory testDataDir |> ignore

let http = new Net.Http.HttpClient()

for file in testFiles do
    let path = Path.Combine(testDataDir, file)

    if not (File.Exists path) then
        printfn "Downloading %s" file
        // Needed because files are in subdirectories
        let dir = Path.GetDirectoryName(path)

        if not (Directory.Exists dir) then
            Directory.CreateDirectory dir |> ignore

        let url =
            $"https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/{file}"

        let csv = http.GetStringAsync(url).Result
        File.WriteAllText(path, csv)

http.Dispose()

module Statsmodels =
    let guerry =
        CsvFile
            .Load(Path.Combine(__SOURCE_DIRECTORY__, "TestData/HistData/Guerry.csv"))
            .Rows
        |> Seq.map (fun x ->
            {| Lottery = float x["Lottery"]
               Literacy = float x["Literacy"]
               LogPopulation = x["Pop1831"] |> float |> log |})

    let model = Ols("Lottery ~ Literacy + LogPopulation", guerry)
    let results = model.fit ()

    let tol = 1e-6

    [<Fact>]
    let ``df_model`` () =
        [ model.df_model; results.df_model ] |> should matchList [ 2; 2 ]

    [<Fact>]
    let ``df_resid`` () =
        [ model.df_resid; results.df_resid ] |> should matchList [ 83; 83 ]

    [<Fact>]
    let ``endog_names`` () =
        [ model.endog_names; results.endog_names ]
        |> should matchList [ "Lottery"; "Lottery" ]

    [<Fact>]
    let ``exog_names`` () =
        [ model.exog_names; results.exog_names ]
        |> List.map (fun xs -> xs |> shouldEqual [| "Intercept"; "Literacy"; "LogPopulation" |])

    [<Fact>]
    let ``kconstant`` () = model.k_constant |> shouldEqual 1

    [<Fact>]
    let ``R-squared`` () =
        results.rsquared |> should (equalWithin tol) 0.3484706112599609

    [<Fact>]
    let ``R-squared adj`` () =
        results.rsquared_adj |> should (equalWithin tol) 0.3327711079168274

    [<Fact>]
    let ``fvalue`` () =
        results.fvalue |> should (equalWithin tol) 22.196282496565242
        results.f_pvalue |> should (equalWithin tol) 1.8978480126470132e-08

    [<Fact>]
    let ``nobs`` () = results.nobs |> shouldEqual 86

    [<Fact>]
    let ``params`` () =
        let expected =
            [ "Intercept", 246.434135; "Literacy", -0.488923; "LogPopulation", -31.311392 ]
            |> Map

        let actual = results.coefs

        actual.Keys |> should equalSeq expected.Keys
        actual["Intercept"] |> should (equalWithin tol) expected["Intercept"]
        actual["Literacy"] |> should (equalWithin tol) expected["Literacy"]
        actual["LogPopulation"] |> should (equalWithin tol) expected["LogPopulation"]

    [<Fact>]
    let ``tvalues`` () =
        let expected =
            [ "Intercept", 6.994511; "Literacy", -3.832038; "LogPopulation", -5.238842 ]
            |> Map

        let actual = results.tvalues

        actual.Keys |> should equalSeq expected.Keys
        actual["Intercept"] |> should (equalWithin tol) expected["Intercept"]
        actual["Literacy"] |> should (equalWithin tol) expected["Literacy"]
        actual["LogPopulation"] |> should (equalWithin tol) expected["LogPopulation"]

    [<Fact>]
    let ``pvalues`` () =
        let expected =
            [ "Intercept", 6.260771e-10
              "Literacy", 2.462102e-04
              "LogPopulation", 1.202925e-06 ]
            |> Map

        let actual = results.pvalues

        actual.Keys |> should equalSeq expected.Keys
        actual["Intercept"] |> should (equalWithin tol) expected["Intercept"]
        actual["Literacy"] |> should (equalWithin tol) expected["Literacy"]
        actual["LogPopulation"] |> should (equalWithin tol) expected["LogPopulation"]

    [<Fact>]
    let ``resid`` () =
        let expected =
            [| -4.281116
               11.892685
               4.333105
               14.154287
               18.493337
               19.346356
               -5.185455
               10.639463
               -17.162269
               -4.501280 |]

        let actual = results.resid[..9]

        actual |> should haveLength expected.Length

        (expected, actual)
        ||> Array.zip
        |> Array.map (fun (exp, act) -> act |> should (equalWithin tol) exp)

    open System.Text.RegularExpressions

    [<Fact>]
    let ``summary`` () =
        let smry = Regex.Replace(results.summary (), @" +", "|")

        let expected =
            [ "Dep.|Variable:|Lottery"
              "Model:|OLS"
              "Method:|Least|Squares"
              "No.|Observations:|86"
              "Df|Model:|2"
              "Df|Residuals:|83"
              "R-squared:|0.348"
              "Adj.|R-squared:|0.333"
              "F-statistic:|22."
              "Prob|(F-statistic):|1.9e-08"
              "Intercept|246.4341|35.233|6.995"
              "176.358|316.510"
              "Literacy|-0.4889|0.128|-3.832"
              "-0.743|-0.235" ]

        for x in expected do
            printfn $"{x}"
            smry |> should haveSubstring x

    [<Fact>]
    let ``fail when variable name is not in collection`` () =
        let msg =
            "Your data does not have a field named LOTTERY. Check spelling in your formula."

        (fun () -> Ols("LOTTERY ~ Literacy + LogPopulation", guerry) |> ignore)
        |> should (throwWithMessage msg) typeof<System.Exception>

module R =
    let iris =
        CsvFile
            .Load(Path.Combine(__SOURCE_DIRECTORY__, "TestData/datasets/iris.csv"))
            .Rows
        |> Seq.map (fun x ->
            {| SepalLength = float x["Sepal.Length"]
               SepalWidth = float x["Sepal.Width"]
               PetalLength = float x["Petal.Length"]
               PetalWidth = float x["Petal.Width"]
               Species = x["Species"] |})

    let m1 = Ols("SepalLength ~ PetalLength + PetalWidth", iris |> Seq.take 100)
    let m1Fit = m1.fit ()

    [<Fact>]
    let ``m1 tvalues`` () =
        let expected =
            [ "Intercept", 33.3177497; "PetalLength", 3.2756232; "PetalWidth", -0.5057872 ]
            |> Map

        let actual = m1Fit.tvalues

        actual.Keys |> should equalSeq expected.Keys
        actual["Intercept"] |> should (equalWithin 1e-6) expected["Intercept"]
        actual["PetalLength"] |> should (equalWithin 1e-6) expected["PetalLength"]
        actual["PetalWidth"] |> should (equalWithin 1e-6) expected["PetalWidth"]

    [<Fact>]
    let ``m1 predict`` () =

        let expected = [ 6.513485; 6.232809; 6.538239; 6.461462; 6.479082 ]
        let actual = m1Fit.predict (iris |> Seq.skip 100 |> Seq.take 5)

        for (exp, act) in Seq.zip expected actual do
            act |> should (equalWithin 1e-6) exp
