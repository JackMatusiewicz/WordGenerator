open System.Text

type Word = string
type Occurrences = Map<char, int>
type Prefix = {First : char; Second : char}
type Trigram = char*char*char
type TrigramStore = Map<Prefix, Occurrences>

let (<!>) = List.map

let pick (randomRange : int -> int) (occ : Occurrences) : char =
    let data = Map.toList occ |> List.sortBy fst
    let total = data |> List.map snd |> List.sum
    let chosenIndex = randomRange (total + 1)

    let rec find (acc : int) (vals : (char*int) list) =
        match vals with
        | [] -> failwith "TODO - force to never happen"
        | (c,v)::t ->
            let newAcc = acc - v
            if newAcc <= 0 then
                c
            else find newAcc t
    find chosenIndex data

let flip (f : 'a -> 'b -> 'c) =
    fun b a -> f a b

let apply (fs : ('a -> 'b) list) (xs : 'a list) =
    let rec calc (acc : 'b list list) fs =
        match fs with
        | [] ->
            acc
            |> List.rev
            |> List.concat
        | (h::t) ->
            calc ((h <!> xs) :: acc) t
    calc [] fs
let (<*>) = apply

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

let constructTrigrams (w : Word) : Trigram list =

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
    | a::b::t -> calc [] (a,b) t
    | _ -> failwith "Will never happen"

let add ((f,s,v) : Trigram) (m : TrigramStore) : TrigramStore =
    let prefix = {First = f; Second = s}
    Map.tryFind prefix m
    |> Option.map (addToOccurrences v)
    |> orDefault (Map.add v 1 (Map.empty))
    |> (fun v -> Map.add prefix v m)

let addSmoothing (m : TrigramStore) : TrigramStore =
    makeTrigram
    <!> ('$' :: ['a' .. 'z'])
    <*> ('$' :: ['a' .. 'z'])
    <*> ('@' :: ['a' .. 'z'])
    |> List.filter (fun (a,b,_) -> not (a <> '$' && b = '$'))
    |> List.fold (flip add) m

let constructModel (words : Word list) =
    words
    |> List.map addStartSymbols
    |> List.map addEndSymbol
    |> List.collect constructTrigrams
    |> List.fold (flip add) Map.empty
    |> addSmoothing

let buildName (store : TrigramStore) =
    let sb = (new StringBuilder()).Append("$$")
    let state = {First = '$'; Second = '$'}
    let r = System.Random()
    let next = fun i -> r.Next(0, i)

    let rec build (acc : StringBuilder) (state : Prefix) =
        let occ = Map.find state store
        let c = pick next occ
        if c = '@' then
            sb.ToString().TrimStart([|'$'|])
        else
            let newState = {First = state.Second; Second = c}
            build (acc.Append(c)) newState
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
    let name = buildName model
    printfn "%s" name
    0
