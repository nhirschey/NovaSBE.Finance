module NovaSBE.Finance.Formula

open System

module internal List =
    /// Partitions list into an initial sequence (while the
    /// specified predicate returns true) and a rest of the list.
    let partitionWhile p input =
        let rec loop acc =
            function
            | hd :: tl when p hd -> loop (hd :: acc) tl
            | rest -> List.rev acc, rest

        loop [] input

    /// Partitions list into an initial sequence (while the specified predicate
    /// returns true) and a rest of the list. The predicate gets the entire
    /// tail of the list and can perform lookahead.
    let partitionWhileLookahead p input =
        let rec loop acc =
            function
            | hd :: tl when p (hd :: tl) -> loop (hd :: acc) tl
            | rest -> List.rev acc, rest

        loop [] input

    /// Partitions list into an initial sequence (while the
    /// specified predicate returns 'false') and a rest of the list.
    let partitionUntil p input = partitionWhile (p >> not) input

    /// Partitions list into an initial sequence (while the
    /// specified predicate returns 'false') and a rest of the list.
    let partitionUntilLookahead p input =
        partitionWhileLookahead (p >> not) input

module Char =
    let (|WhiteSpace|_|) input =
        match input with
        | [] -> Some input
        | x :: _xs -> if Char.IsWhiteSpace x then Some input else None

    let (|NotWhiteSpace|_|) input =
        match input with
        | WhiteSpace _ -> None
        | _ -> Some input

    let (|AlphaNum|_|) input =
        match input with
        | [] -> None
        | x :: xs -> if Char.IsLetterOrDigit x then Some(x, xs) else None

    let toStringList (input: char list) =
        input |> List.map string |> String.concat ""

let (|Yvar|_|) input =
    match input with
    | Char.AlphaNum _ ->
        let yvar, rest = List.partitionWhile Char.IsLetterOrDigit input

        let rec toTilde input =
            match input with
            | '~' :: rest -> Some rest
            | Char.WhiteSpace(x :: xs) -> toTilde xs
            | _ -> None

        match toTilde rest with
        | None -> None
        | Some xvars -> Some(Char.toStringList yvar, xvars)
    | _ -> None

let (|Xvar|_|) input =
    match input with
    | Char.AlphaNum _ ->
        let xvar, rest = List.partitionWhile Char.IsLetterOrDigit input
        Some(Char.toStringList xvar, rest)
    | _ -> None

let (|NoIntercept|_|) input =
    match input with
    | '-' :: rest ->
        match List.partitionWhile Char.IsWhiteSpace rest with
        | _ws, '1' :: '.' :: '0' :: rest
        | _ws, '1' :: '.' :: rest
        | _ws, '1' :: rest -> Some rest
        | _ -> None
    | _ -> None


let parseFormula (formula: string) =
    let rec loop yvar xvars intercept input =
        match yvar, xvars, input with
        | Some yvar, _, [] ->
            let xvars = xvars |> List.distinct |> List.rev

            if xvars |> List.contains yvar then
                failwith $"Y variable {yvar} is also an X variable"

            yvar, xvars, intercept
        | _, _, Char.WhiteSpace(_ :: xs) -> loop yvar xvars intercept xs
        | None, [], Yvar(yvar, xvars) -> loop (Some yvar) [] intercept xvars
        | None, _, _ -> failwith "No Y variable"
        | Some _, _, NoIntercept(rest) -> loop yvar xvars false rest
        | Some _, _, Xvar(xvar, rest) -> loop yvar (xvar :: xvars) intercept rest
        | Some _, _, '+' :: rest -> loop yvar xvars intercept rest
        | _, _, x :: xs -> failwith $"unhandled char: {x}"

    loop None [] true (formula.ToCharArray() |> Array.toList)
