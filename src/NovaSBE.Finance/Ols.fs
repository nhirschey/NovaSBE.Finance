module NovaSBE.Finance.Ols

open System
open FSharp.Stats
open FSharp.Stats.Algebra
open FSharp.Stats.Distributions

open DiffSharp

// The config seems to get ignored so I manually set float64 below
dsharp.config (dtype = Dtype.Float64, backend = Backend.Reference)

open NovaSBE.Finance.Formula

let internal forg prec x =
    if prec = 3 then
        // for 3 decimals
        if (abs (x) >= 1e4) || (abs (x) < 1e-4) then
            sprintf "%9.3g" x
        else
            sprintf "%9.3f" x
    elif prec = 4 then
        if (abs (x) >= 1e4) || (abs (x) < 1e-4) then
            sprintf "%10.4g" x
        else
            sprintf "%10.4f" x
    else
        failwith $"`prec` argument must be either 3 or 4, not {prec}"

type CovType = Nonrobust

type RegressionResults(df_model: int, df_resid: int, endog, exog, endog_names, exog_names, intercept, covtype) =
    let x = dsharp.tensor (exog, dtype = Dtype.Float64)
    let y = dsharp.tensor (endog, dtype = Dtype.Float64)
    let nobs' = y.nelement
    let coefs' = (x.transpose().matmul(x).inv ()).matmul (x.transpose().matmul (y))
    let yhat = x.matmul (coefs')

    let ss =
        if intercept then
            let ybar = y.mean ()
            (y - ybar).pow(2.0).sum ()
        else
            y.pow(2.0).sum ()

    let errors = y - yhat
    let ssr' = errors.pow(2.0).sum () |> float
    let ess' = float ss - ssr'
    let sigma2_hat = ssr' / float df_resid

    let stderrors = (sigma2_hat * x.transpose().matmul(x).inv ()).diagonal().pow (0.5)
    let tv = coefs' / stderrors

    let studentT = Continuous.StudentT.Init 0.0 1.0 df_resid

    let pvalues' =
        tv.toArray1D () |> Array.map (fun t -> 2.0 * (1.0 - (studentT.CDF(abs t))))

    let fvalue' = ((ss - ssr') / float df_model) / (ssr' / float df_resid) |> float

    let f_pvalue' =
        let fdist = ContinuousDistribution.f df_model df_resid
        1.0 - fdist.CDF fvalue'

    let r2 = 1.0 - ssr' / ss |> float
    let r2adj = 1.0 - (float nobs' - 1.0) / float df_resid * (1.0 - r2) |> float
    /// Model degress of freedom
    member _.df_model = df_model
    member _.df_resid = df_resid
    member _.endog_names = endog_names
    member _.exog_names = exog_names
    /// The explained sum of squares
    member _.ess = ess'
    /// The F-statistic of the model
    member _.fvalue = fvalue'
    /// The p-value of the F-statistic
    member _.f_pvalue = f_pvalue'
    /// The fitted values of the model
    member _.fittedvalues = yhat.toArray1D<float> ()
    /// The mean squared error of the model
    member _.mse_model = ess' / float df_model
    /// The mean squared error of the residuals
    member _.mse_resid = ssr' / float df_resid
    /// Total mean squared error
    member _.mse_total = float (y.pow(2.0).sum ()) / float nobs'
    /// The number of observations
    member _.nobs = nobs'
    /// Two-sided p-values for the model coefficients
    member _.pvalues = (exog_names, pvalues') ||> Array.zip |> Map
    /// The model residuals
    member _.resid = errors.toArray1D<float> ()
    /// The R-squared of the model
    member _.rsquared = r2
    /// The adjusted R-squared of the model
    member _.rsquared_adj = r2adj
    /// Sum of squared residuals
    member _.ssr = ssr'
    /// The t-statistics of the model coefficients
    member _.tvalues = (exog_names, tv.toArray1D<float> ()) ||> Array.zip |> Map
    /// The estimated model coefficients
    member _.coefs = (exog_names, coefs'.toArray1D<float> ()) ||> Array.zip |> Map

    /// Summarise the regression results
    member _.summary(?yname: string, ?xname: seq<string>, ?title: string, ?alpha: float, ?slim: bool) =
        let yname = defaultArg yname endog_names
        let xname = defaultArg xname exog_names
        let title = defaultArg title "OLS Regression Results"
        let alpha = defaultArg alpha 0.05
        let slim = defaultArg slim false

        let padWidth (rows: list<list<string>>) =
            //let rowWidths =
            //    [ for row in rows do
            //        [ for col in row do
            //            col.Length ]
            //        |> List.sum ]
            //let maxWidth = rowWidths |> List.max
            let nCols = rows |> List.map List.length |> List.distinct |> List.exactlyOne

            let colMaxWidth =
                [ for i = 0 to nCols - 1 do
                      rows |> List.map (fun row -> row[i]) |> List.map String.length |> List.max ]

            let pad max (s: string) =
                String.init (max - s.Length) (fun _ -> " ")

            let colSep = "    "

            [ for row in rows do
                  [ for i = 0 to nCols - 1 do
                        let padding = pad colMaxWidth[i] row[i]
                        if i = 0 then row[i] + padding else padding + row[i] ]
                  |> String.concat colSep ]

        let top_left =
            [ [ "Dep. Variable:"; yname ]
              [ "Model:"; "OLS" ]
              if not slim then
                  [ "Method:"; "Least Squares" ]
              if not slim then
                  [ "Date:"; DateTime.Now.ToString("yyyy-MM-dd") ]
              if not slim then
                  [ "Time:"; DateTime.Now.ToString("HH:mm:ss") ]
              [ "No. Observations:"; nobs'.ToString() ]
              if not slim then
                  [ "Df Residuals:"; df_resid.ToString() ]
              if not slim then
                  [ "Df Model:"; df_model.ToString() ]
              [ "Covariance Type:"; covtype.ToString() ] ]
            |> padWidth

        let top_right =
            [ [ "R-squared:"; $"%8.3f{r2}" ]
              [ "Adj. R-squared:"; $"%8.3f{r2adj}" ]
              [ "F-statistic:"; $"%8.4f{fvalue'}" ]
              [ "Prob (F-statistic):"; $"%6.3g{f_pvalue'}" ] ]
            |> padWidth

        let top =
            [ for i = 0 to top_left.Length - 1 do
                  if i <= top_right.Length - 1 then
                      [ top_left[i]; top_right[i] ]
                  else
                      [ top_left[i]; "" ] ]
            |> padWidth

        let criticalT =
            ContinuousDistribution.getCriticalTValue df_resid alpha ContinuousDistribution.TwoTailed

        let table_params =
            [ [ ""
                "coef"
                "std err"
                "t"
                "P>|t|"
                $"[{alpha / 2.0}"
                $"{1.0 - alpha / 2.0}]" ]
              for i = 0 to exog_names.Length - 1 do
                  [ exog_names[i]
                    forg 4 (float coefs'[i])
                    forg 3 (float stderrors[i])
                    forg 3 (float tv[i])
                    sprintf "%6.3g" pvalues'[i]
                    forg 3 (coefs'[i] - criticalT * stderrors[i] |> float)
                    forg 3 (coefs'[i] + criticalT * stderrors[i] |> float) ] ]
            |> padWidth

        let border = String.init table_params[0].Length (fun _ -> "=")
        let table = border :: top @ border.Replace("=","-") :: table_params @ [ border ]

        table |> String.concat "\n"


type FitMethod =
    | QR
    | PINV

/// <summary>Create a model from a formula and collection of records</summary>
type Ols<'Record>(formula: string, data: 'Record seq) =
    let yvar, xvars, intercept = parseFormula formula
    let schema = typeof<'Record>
    let fields = Reflection.FSharpType.GetRecordFields schema

    let getField variable =
        fields
        |> Array.find (fun f -> f.Name = variable)
        |> Reflection.FSharpValue.PreComputeRecordFieldReader

    let xfields = xvars |> Seq.map getField |> Seq.toArray
    let yfield = getField yvar

    let exog =
        data
        |> Seq.map (fun row ->
            let xf = xfields |> Seq.map (fun getter -> getter row :?> float)

            if not intercept then xf else Seq.append [ 1. ] xf
            |> Seq.toArray)
        |> Seq.toArray

    let endog = data |> Seq.map (fun row -> yfield row :?> float) |> Seq.toArray
    let df_model' = xvars.Length
    let df_resid' = exog.Length - (exog[0].Length)
    let endog_names' = yvar

    let exog_names' =
        if intercept then "Intercept" :: xvars else xvars
        |> List.toArray

    member _.df_model = df_model'
    member _.df_resid = df_resid'
    member _.endog_names = endog_names'
    member _.exog_names = exog_names'
    member _.k_constant = if intercept then 1 else 0

    member _.fit() =
        RegressionResults(df_model', df_resid', endog, exog, endog_names', exog_names', intercept, CovType.Nonrobust)
