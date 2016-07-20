namespace FactorioRecipes

type EntityAmount =
    {amount:float
     name:string
     type':string}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EntityAmount =
    let empty =
        {amount=0.0
         name=""
         type'=""}

    let setValue =
        function
        | Value [(Float 1.0, String id);
                 (Float 2.0,Float count)] ->
            Some {amount = count
                  name = id
                  type' = ""}

        | Value [(String "amount", Float count);
                 (String "name", String name);
                 (String "type", String type')] ->
            Some {amount = count
                  name = name
                  type' = type'}

        | _ -> None
            

type Recipe =
    {``type``:string
     name:string
     subgroup:string
     category:string option
     enabled:bool
     energy_required:float
     ingredients:EntityAmount list
     result_count:float
     hidden:bool
     result:EntityAmount list }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Recipe =
    let empty =
        {``type`` = ""
         name = ""
         subgroup = ""
         enabled = false
         category = None
         energy_required = 0.0
         ingredients = []
         result_count = 1.0
         hidden = false
         result = [] }
        
    let setValue r (x:LuaType*LuaType) =
        match x with
        | (String "type", String x) ->
            {r with ``type``=x}

        | (String "name", String x) ->
            {r with name=x}

        | (String "subgroup", String x) ->
            {r with subgroup=x}
        
        | (String "enabled", Bool x) ->
            {r with enabled=x}
        | (String "enabled", String "true") ->
            {r with enabled=true}
        | (String "enabled", String "false") ->
            {r with enabled=false}
        
        | (String "category", String x) ->
            {r with category=Some x}
        
        | (String "energy_required", Float x) ->
            {r with energy_required=x}
        
        | (String "ingredients", Value x) ->
            let i = List.choose (snd >> EntityAmount.setValue) x
            {r with ingredients=i}
        
        | (String "result", String x) ->
            {r with result=[{amount=1.0; name=x; type'=""}]}

        | (String "results", Value x) ->
            let res = List.choose (snd >> EntityAmount.setValue) x
            {r with result=res}

        | (String "result_count", Float x) ->
            {r with result_count=x}

        | (String "hidden", Bool x) ->
            {r with hidden=x}

        | (String "icon", _) -> r
        | (String "order", _) -> r
        | (String "main_product", _) -> r
        
        | _ -> r
