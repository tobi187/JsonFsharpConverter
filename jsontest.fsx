#r "nuget: Chiron, 6.3.1"

open Chiron

open System.Text.Json
open System.Text.Json.Nodes

let jsonFile =
    """{"menu":{"id":"file","value":"File","popup":{"menuitem":[{"value":null,"onclick":"CreateDoc()"},{"value":"Open","onclick":"OpenDoc()"},{"value":"Save","onclick":"SaveDoc()"},{"value":"Save","onclick":"SaveDoc()"},{"value":"Schnick","onclick":"Schnack()"}]}}}"""

let parsed = Json.parse jsonFile

type Elem = { name: string; dType: string }

type Group =
    { name: string
      dType: string
      kids: Elem list }

let rec unpack parsedJson result = ()


let js = JsonSerializer.Deserialize jsonFile :> JsonValue

js.GetValueKind |> printfn "Type: %A"

let detectJsonType (root: JsonValue) =
    let pType =
        match root.GetValueKind() with
        | JsonValueKind.Array -> "array"
        | JsonValueKind.Object -> "object"
        | JsonValueKind.False
        | JsonValueKind.True -> "bool"
        | JsonValueKind.Null -> "obj option"
        | JsonValueKind.Number -> "int"
        | JsonValueKind.String -> "string"
        | JsonValueKind.Undefined -> failwith "undefined"
        | uint -> failwith (sprintf "JsonValue Match Whati is going on %A" (root.GetValue()))

    { name = root.GetPropertyName()
      dType = pType }
