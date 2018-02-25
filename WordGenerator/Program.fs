open System.Text

open WordGenerator
open Result
open ListExtension

let bimap (f : 'a -> 'b) (g : 'c -> 'd) ((a,b) : 'a*'c) =
    (f a),(g b)

let pick (randomRange : int -> int) (occ : Occurrences) : Result<string, char> =
    let data = Map.toList occ |> List.sortBy fst
    let total = data |> List.sumBy snd
    let chosenIndex = randomRange (total + 1)

    let rec find (acc : int) (vals : (char*int) list) =
        match vals with
        | [] -> Failure "Unable to pick element"
        | (c,v)::t ->
            let newAcc = acc - v
            if newAcc <= 0 then
                Success c
            else find newAcc t
    find chosenIndex data

let flip (f : 'a -> 'b -> 'c) =
    fun b a -> f a b

let makeTrigram a b c : Trigram = a,b,c

let addStartSymbols (w : string) =
    "$$" + w

let addEndSymbol (w : string) =
    w + "@"

let addToOccurrences (c : char) (occ : Occurrences) : Occurrences =
    match Map.tryFind c occ with
    | None ->
        Map.add c 1 occ
    | Some n ->
        Map.add c (n + 1) occ

let orDefault (v : 'a) (a : 'a option) =
    match a with
    | Some b -> b
    | None -> v

let constructTrigrams (w : Word) : Result<string, Trigram list> =

    let rec calc
        (acc : Trigram list)
        ((a,b) : char*char)
        (rem : char list) =

        match rem with
        | [] -> List.rev acc
        | h::t ->
            calc ((a,b,h)::acc) (b,h) t

    let chars =
        w.ToCharArray()
        |> Array.toList

    match chars with
    | a::b::c::t -> Success <| calc [] (a,b) (c::t)
    | _ -> Failure "Not enough characters to construct trigrams."

let add ((f,s,v) : Trigram) (m : TrigramStore) : TrigramStore =
    let prefix = {First = f; Second = s}
    Map.tryFind prefix m
    |> Option.map (addToOccurrences v)
    |> orDefault (Map.add v 1 (Map.empty))
    |> (fun v -> Map.add prefix v m)

let scaleStore (m : TrigramStore) =
    let scale (occ : Occurrences) =
        occ
        |> Map.toList
        |> List.map (bimap id ((*) 500))
        |> Map.ofList

    m
    |> Map.toList
    |> List.map (fun (a,b) -> a, (scale b))
    |> Map.ofList

let addSmoothing (m : TrigramStore) : TrigramStore =
    makeTrigram
    |> (flip List.map) ('$' :: ['a' .. 'z'])
    |> (flip listApply) ('$' :: ['a' .. 'z'])
    |> (flip listApply) ('@' :: ['a' .. 'z'])
    |> List.filter (fun (a,b,_) -> not (a <> '$' && b = '$'))
    |> List.fold (flip add) m

let constructModel (words : Word list) : Result<string, TrigramStore> =
    words
    |> List.map addStartSymbols
    |> List.map addEndSymbol
    |> Result.traverse constructTrigrams
    |> Result.map (List.concat)
    |> Result.map (List.fold (flip add) Map.empty)
    |> Result.map (scaleStore >> addSmoothing)

let buildName (store : TrigramStore) : Result<string, string> =
    let sb = (new StringBuilder()).Append("$$")
    let state = {First = '$'; Second = '$'}
    let r = System.Random()
    let next = fun i -> r.Next(0, i)

    let rec build (acc : StringBuilder) (state : Prefix) =
        let occ = Map.find state store
        pick next occ
        >>= (fun c ->
                if c = '@' then
                    Success (sb.ToString().TrimStart([|'$'|]))
                else
                    let newState = {First = state.Second; Second = c}
                    build (acc.Append(c)) newState)
    build sb state

[<EntryPoint>]
let main argv =
    let model =
        [
            "durond"
            "dorum"
            "kilond"
            "brorak"
            "thum"
            "gomond"
            "hatil"
            "fimo"
            "baltil"
            "norain"
            "ovrund"
            "dlin"
            "munur"
            "garil"
            "dordin"
            "calri"
            "simdri"
            "hertri"
            "biltri"
            "ovum"
            "chalain"
            "farum"
            "bolond"
            "bilrund"
            "bofond"
            "normin"
            "fimil"
            "thinin"
            "dgol"
            "dtri"
            "storrak"
            "bilunn"
            "bilbar"
            "harak"
            "ovkon"
            "dorni"
            "runbar"
            "herlin"
        ]
        |> constructModel
    Result.bind model buildName
    |> Result.iter (printfn "%s")
    0
