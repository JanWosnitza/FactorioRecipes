module FactorioRecipes.Program

open NLua
open System.IO
open System
open System.Diagnostics

type Data() =
    let mutable l = []
    member this.List = l
    member this.extend( x:LuaTable ) = l <- LuaType.fromLuaTable x @ l        

let getRecipeFileContents (factorioDir) =
    use lua = new Lua()
    let data = Data()
    lua.[ "data" ] <- data

    Directory.GetFiles( Path.Combine( factorioDir, "data", "base", "prototypes", "recipe" ) )
    |> Seq.where (fun x -> Path.GetExtension( x ).ToLower() = ".lua" )
    |> Seq.iter (lua.DoFile >> ignore)

    data.List

let parseRecipes (list) =
    list
    |> List.filter LuaType.isValue
    |> List.map (LuaType.value >> List.fold Recipe.setValue Recipe.empty)

let toConnections (recipes) =
    let exclude = [
        //"rocket-silo"
        //"satellite"
        "small-plane"
        "crude-oil-barrel"
        "empty-crude-oil-barrel"
        "fill-crude-oil-barrel"
    ]

    recipes
    |> Seq.where (fun x -> List.exists ((=)x.Name) exclude |> not)
    |> Seq.collect (fun x ->
        x.Ingredients
        |> List.map (fun i ->
            let coeff = if i.Type = "fluid" then 1.0 else 1.0
            (i.Name, x.Name, i.Amount / x.ResultCount * coeff )))
    |> Seq.append [("coal", "iron-plate", 1.0/25.0);
                    ("coal", "copper-plate", 1.0/25.0)]
    |> Seq.choose (function
        | (s, "copper-cable", a) -> None
        | ("copper-cable", t, a) -> Some ("copper-plate", t, a/2.0)
        | x -> Some x)
    |> Seq.toList

let shellOpen (x) =
    ProcessStartInfo( x, UseShellExecute = true, Verb = "OPEN" )
    |> Process.Start

[<EntryPoint; STAThread>]
let Main (args) =
    let factorioDir = args.[0]

    let get name select arr =
        arr
        |> Seq.where (fun (f, t, _) -> select (f, t) = name )
        |> Seq.toArray

    let rec accumulateFromRight name xs =
        let cs = xs |> get name fst
        let count =
            if cs.Length = 0 then 1.0
            else (cs |> Seq.sumBy (fun (f, t, a) -> accumulateFromRight t xs * a ))
        let additional =
            match name with
            (*| "basic-transport-belt" -> 1000.0
            | "science-pack-1" -> 2000.0
            | "science-pack-2" -> 2000.0
            | "science-pack-3" -> 500.0
            | "alien-science-pack" -> 250.0*)
            | _ -> 1.0
        count + additional
        
    let connections =
        getRecipeFileContents factorioDir
        |> List.map snd
        |> parseRecipes
        |> toConnections

    let accumulated =
        connections
        |> Seq.map (fun (f, t, a) ->
            let a' = accumulateFromRight t connections
            (f, t, a * a'))
        |> Seq.toArray

    let sum = accumulated |> Seq.sumBy (fun (_, _, a) -> a)

    accumulated
    |> Seq.map (fun (f, t, a) -> (f, t, a/sum, sprintf "%.1f%%" (a/sum*100.0)))
    |> Seq.sortBy (fun (_, _, a,_ ) -> -a)
    |> Seq.take 21
    |> SankeyHtml.safe "recipes"
    //|> shellOpen
    |> ignore

    if System.Diagnostics.Debugger.IsAttached then
        printfn "Press any key to continue..."
        Console.ReadKey() |> ignore

    0