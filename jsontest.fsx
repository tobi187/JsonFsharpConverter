open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Collections.Generic

let jsonFile =
    """{"menu":{"id":"file","value":"File","popup":{"menuitem":[{"value":null,"onclick":"CreateDoc()"},{"value":"Open","onclick":"OpenDoc()"},{"value":"Save","onclick":"SaveDoc()"},{"value":"Save","onclick":"SaveDoc()"},{"value":"Schnick","onclick":"Schnack()"}]}}}"""


type Elem = 
    { name: string; dType: string }
    with 
    member this.isOption = this.dType.EndsWith "option"
    member this.isUnknown = this.dType = "obj option"
        

[<CustomEquality;NoComparison>]
type Group =
    { name: string
      dType: string
      kids: Elem list
      internalHash : string}

    override this.Equals other =
        match other with
        | :? Group as g -> g.internalHash = this.internalHash
        | _ -> false

    override this.GetHashCode () =
        this.internalHash.GetHashCode ()


let getGroupHash g =
    let strBuilder = StringBuilder ()

    strBuilder.Append g.name |> ignore
    strBuilder.Append ":" |> ignore
    
    g.kids
    |> List.map (_.name)
    |> List.toArray
    |> fun arr -> strBuilder.AppendJoin (",", arr)
    |> ignore

    strBuilder.ToString ()

let seen = new HashSet<Group> ()

let rec unpack parsedJson result = ()

let updateType (e1 : Elem, e2: Elem) =
    // case
        // same type -> any,
        // one x option, other x or option -> one with x option
        // one x option, other type y -> ignore for now
        // one x, other option -> x + option
    match e1, e2 with
    | f, s when f.dType = s.dType -> e1
    | f, _ when f.isOption && not f.isUnknown -> f
    | _, s when s.isOption && not s.isUnknown -> s
    | f, s when f.isUnknown -> { f with dType = $"{s.dType} option" } 
    | f, s when s.isUnknown -> { s with dType = $"{f.dType} option" } 
    | x, y -> failwith (sprintf "%A - %A" x y)

let update old neww =
    let tcorr = {
        neww with kids = neww.kids
                        |> List.zip old.kids
                        |> List.map (updateType) 
    }

    seen.Remove old || seen.Add tcorr

let addOrUpdate el =
    match seen.TryGetValue el with
    | true, x -> update x el
    | false, _ -> seen.Add el

let js = JsonSerializer.Deserialize jsonFile :> JsonValue

js.GetValueKind |> printfn "Type: %A"

let detectJsonType (root: JsonValue) =
    let pType =
        match root.GetValueKind() with
        | JsonValueKind.Array -> "array" // array list seq ?
        | JsonValueKind.Object -> "object" // insert object name
        | JsonValueKind.False
        | JsonValueKind.True -> "bool"
        | JsonValueKind.Null -> "obj option"  // other type option
        | JsonValueKind.Number -> "int" // int int64 decimal float 
        | JsonValueKind.String -> "string"
        | JsonValueKind.Undefined -> failwith "undefined"
        | uint -> failwith (sprintf "JsonValue Match Whati is going on %A" (root.GetValue()))

    { name = root.GetPropertyName()
      dType = pType }

let detectJsonTypeP (root: JsonProperty) =
    let pType =
        match root.Value.ValueKind with
        | JsonValueKind.Array -> root.Name + " list"  // "array" // array list seq ?
        | JsonValueKind.Object -> root.Name // "object" // insert object name
        | JsonValueKind.False
        | JsonValueKind.True -> "bool"
        | JsonValueKind.Null -> "obj option"  // other type option
        | JsonValueKind.Number -> "int" // int int64 decimal float 
        | JsonValueKind.String -> "string"
        | JsonValueKind.Undefined -> failwith "undefined"
        | uint -> failwith (sprintf "JsonValue Match Whati is going on %A" (root.Value))

    { name = root.Name
      dType = pType }

let unwrapArray (value: JsonValue) (name: string) =
    0

let unwrapObject (value: JsonValue) (name:string) =
    let a = value.GetValue() :> JsonElement
    let fields = 
        a.EnumerateObject()
        |> Seq.map (detectJsonTypeP)

    let g = {
        name = name;
        dType = name;
        kids = fields;
        internalHash = ""
    }

    let gg = { g with internalHash = getGroupHash g }

    addOrUpdate gg |> printfn "Update successful: %b"

    let objs =
        a.EnumerateObject()
        |> Seq.filter (fun s -> s.Value.ValueKind = JsonValueKind.Object)

