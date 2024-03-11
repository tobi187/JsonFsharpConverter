#r "nuget: Chiron, 6.3.1"

open Chiron

open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Collections.Generic

let jsonFile =
    """{"menu":{"id":"file","value":"File","popup":{"menuitem":[{"value":null,"onclick":"CreateDoc()"},{"value":"Open","onclick":"OpenDoc()"},{"value":"Save","onclick":"SaveDoc()"},{"value":"Save","onclick":"SaveDoc()"},{"value":"Schnick","onclick":"Schnack()"}]}}}"""

let parsed = Json.parse jsonFile


type Elem = { name: string; dType: string }

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

let update old neww =
    seen.Remove old || seen.Add neww

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