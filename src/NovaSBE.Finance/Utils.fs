module NovaSBE.Finance.Utils

open System
open System.Net

let download (inputUrl: string) (outputFile: string) =
    if IO.File.Exists(outputFile) then
        printfn $"The file {outputFile} already exists. Skipping download" 
    else
        use fileStream = IO.File.Create(outputFile)
        use http = new Http.HttpClient()
        use urlStream = http.GetStreamAsync(inputUrl).Result
        urlStream.CopyTo(fileStream)
