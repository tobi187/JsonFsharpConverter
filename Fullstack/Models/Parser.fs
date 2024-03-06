
module Parser
open System.Collections.Frozen

let keyWords = Set ["asr";"land";"lor";"lsl";"lsr";"lxor";"mod";"sig";"const";"yield!";"yield";"with";"while";"when";"void";"val";"use!";"use";"upcast";"type";"try";"true";"false";"to ";"then";"struct";"static";"select";"return";"return!";"rec";"public";"private";"override";"or";"open";"new";"of";"null";"not";"namespace";"mutable";"module";"member";"match!";"match";"let!";"let";"lazy";"internal";"interface";"inline";"inherit";"in";"if";"global";"function";"fun";"for";"fixed";"finally";"extern";"exception";"end";"else";"elif";"downto";"downcast";"done";"do";"delegate";"default";"class";"begin";"base";"assert";"as";"and";"abstract"]

type Element = {
    key: string
    dtType: string

}

let toString e = function
    | _ when Set.contains e.key keyWords -> $"    ``{e.key}`` : {e.dtType}"
    | _ -> $"    {e.key} : {e.dtType}" 


let parseJson txt =
    ()

let buildType name elems =
    let str = 
        $"type {name}\n" +
        elems
        |> List.map (toString)
        |> String.concat
    
    str

