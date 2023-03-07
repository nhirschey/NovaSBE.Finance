namespace NovaSBE.Finance

module French =
    open System
    open System.IO
    open System.IO.Compression
    open FSharp.Data

    type Frequency = Daily | Monthly
    type ReturnObs = { Symbol : string; Date : DateTime; Return : float }


    type private FF3Csv = CsvProvider<"Date (string),Mkt-RF,SMB,HML,RF
        19260701,    0.10,   -0.24,   -0.28,   0.009">
    type FF3Obs = 
        { Date : DateTime 
          MktRf : float
          Smb : float 
          Hml : float
          Rf : float 
          Frequency : Frequency } 

    type private FF5Csv = CsvProvider<"Date (string),Mkt-RF,SMB,HML,RMW,CMA,RF
        19260701,    0.10,   -0.24,   -0.28,0.0,1.2,  0.009">

    type FF5Obs = 
        { Date : DateTime 
          MktRf : float
          Smb : float 
          Hml : float
          Rmw : float
          Cma : float
          Rf : float 
          Frequency : Frequency } 

    let private frenchDay x = 
        DateTime.ParseExact(x,
            "yyyyMMdd",
            Globalization.CultureInfo.InvariantCulture)
    let private frenchMonth x = 
        DateTime.ParseExact(x,
            "yyyyMM",
            Globalization.CultureInfo.InvariantCulture)

    let private cache = 
        let today = DateTime.Now
        let nextMonth = today.AddMonths(1)
        let eom = DateTime(nextMonth.Year, nextMonth.Month, 1).AddSeconds(-1.0) 
        Runtime.Caching.createInternetFileCache "French" (eom - today)

    let private getData (dataset:string) =
        match cache.TryRetrieve(dataset) with
        | Some data -> data
        | None ->
            //let dataset = "F-F_Research_Data_Factors_CSV"
            let urlString = $"http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/{dataset}.zip"
            let request = Http.RequestStream(urlString, httpMethod = "GET",headers = [HttpRequestHeaders.Accept HttpContentTypes.Any])
            use archive = new ZipArchive(request.ResponseStream,ZipArchiveMode.Read)
            let file = archive.GetEntry($"{dataset}".Replace("_CSV",".CSV"))
            use reader = new StreamReader(file.Open())
            let data  = reader.ReadToEnd()
            cache.Set(dataset,data)
            data
    let getFF3 frequency =
            let (dataset, dateParser) =
                match frequency with
                | Monthly -> "F-F_Research_Data_Factors_CSV", frenchMonth
                | Daily -> "F-F_Research_Data_Factors_daily_CSV", frenchDay
            let data = new StringReader(getData dataset)
            [| while data.Peek() <> -1 do
                    data.ReadLine() |]
            |> Array.skipWhile(fun line -> not (line.Contains("Mkt-RF")))
            |> Array.skip 1
            |> Array.takeWhile(fun line -> line <> "")
            |> Array.map(fun line -> 
                let parsedLine = FF3Csv.ParseRows(line).[0] 
                { Date = dateParser parsedLine.Date
                  MktRf = float parsedLine.``Mkt-RF`` / 100.0
                  Smb = float parsedLine.SMB / 100.0
                  Hml = float parsedLine.HML / 100.0
                  Rf = float parsedLine.RF / 100.0 
                  Frequency = frequency })

    let getFF5 frequency =
        let (dataset, dateParser) =
            match frequency with
            | Monthly -> "F-F_Research_Data_5_Factors_2x3_CSV", frenchMonth
            | Daily -> "F-F_Research_Data_5_Factors_2x3_daily_CSV", frenchDay
        let data = new StringReader(getData dataset)
        [| while data.Peek() <> -1 do
                data.ReadLine() |]
        |> Array.skipWhile(fun line -> not (line.Contains("Mkt-RF")))
        |> Array.skip 1
        |> Array.takeWhile(fun line -> line <> "")
        |> Array.map(fun line -> 
            let parsedLine = FF5Csv.ParseRows(line).[0] 
            { Date = dateParser parsedLine.Date
              MktRf = float parsedLine.``Mkt-RF`` / 100.0
              Smb = float parsedLine.SMB / 100.0
              Hml = float parsedLine.HML / 100.0
              Rmw = float parsedLine.RMW / 100.0
              Cma = float parsedLine.CMA / 100.0
              Rf = float parsedLine.RF / 100.0 
              Frequency = frequency })
