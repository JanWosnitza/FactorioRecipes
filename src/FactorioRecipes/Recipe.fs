namespace FactorioRecipes

type EntityAmount = {
    Amount : float
    Name : string
    Type : string
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EntityAmount =
    let empty =
        {Amount=0.0
         Name=""
         Type=""}

    let setValue =
        function
        | Value [Float 1.0, String id
                 Float 2.0, Float count] ->
            {Amount = count
             Name = id
             Type = ""
            } |> Some

        | Value [String "amount", Float count
                 String "name"  , String name
                 String "type"  , String type'] ->
            {Amount = count
             Name = name
             Type = type'
            } |> Some 

        | _ -> None
            

type Recipe = {
    Type : string
    Name : string
    Subgroup : string
    Category : string option
    Enabled : bool
    EnergyRequired : float
    Ingredients : EntityAmount list
    ResultCount : float
    Hidden : bool
    Result : EntityAmount list
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Recipe =
    let empty =
        {Type = ""
         Name = ""
         Subgroup = ""
         Enabled = false
         Category = None
         EnergyRequired = 0.0
         Ingredients = []
         ResultCount = 1.0
         Hidden = false
         Result = [] }
        
    let setValue (r:Recipe) (x:LuaType*LuaType) =
        match x with
        | (String "type", String x) ->
            {r with
                Type = x
            }

        | (String "name", String x) ->
            {r with
                Name = x
            }

        | (String "subgroup", String x) ->
            {r with
                Subgroup = x
            }
        
        | (String "enabled", Bool x) ->
            {r with
                Enabled = x
            }

        | (String "enabled", String x) ->
            {r with
                Enabled = (x = "true")
            }

        | (String "category", String x) ->
            {r with
                Category = Some x
            }
        
        | (String "energy_required", Float x) ->
            {r with
                EnergyRequired = x
            }
        
        | (String "ingredients", Value x) ->
            {r with
                Ingredients = List.choose (snd >> EntityAmount.setValue) x
            }
        
        | (String "result", String x) ->
            {r with
                Result =
                    [{Amount = 1.0
                      Name = x
                      Type = ""
                    }]
            }

        | (String "results", Value x) ->
            {r with
                Result = List.choose (snd >> EntityAmount.setValue) x
            }

        | (String "result_count", Float x) ->
            {r with ResultCount = x}

        | (String "hidden", Bool x) ->
            {r with Hidden = x}

        (*
        | (String "icon", _) ->
        | (String "order", _) ->
        | (String "main_product", _) ->
        *)

        | _ -> r
