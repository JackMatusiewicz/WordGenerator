
type Word = string
type Occurrences = Map<char, int>
type Prefix = {First : char; Second : char}
type Trigram = char*char*char
type TrigramStore = Map<Prefix, Occurrences>

let (<!>) = List.map

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

[<EntryPoint>]
let main argv =
    ["muradin"; "gimli"; "gloin"]
    |> constructModel
    |> printfn "%A"
    0
