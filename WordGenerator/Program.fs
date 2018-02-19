
type Word = string
type Occurrences = Map<char, int>
type Prefix = {First : char; Second : char}
type Trigram = char*char*char
type TrigramStore = Map<Prefix, Occurrences>

let (<!>) = List.map

let flip (f : 'a -> 'b -> 'c) =
    fun b a -> f a b

let rec apply (fs : ('a -> 'b) list) (xs : 'a list) =
    match fs with
    | [] -> []
    | f::t -> (List.map f xs) @ (apply t xs)
let (<*>) = apply

let makeTrigram a b c : Trigram = a,b,c

let addStartSymbols (w : string) =
    "$$" + w

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
    match Map.tryFind prefix m with
    | None ->
        let store = Map.add v 1 Map.empty
        Map.add prefix store m
    | Some occ ->
        match Map.tryFind v occ with
        | Some n ->
            let s = Map.add v (n + 1) occ
            Map.add prefix s m
        | None ->
            let s = Map.add v 1 occ
            Map.add prefix s m

let addSmoothing (m : TrigramStore) : TrigramStore =
    makeTrigram
    <!> ('$' :: ['a' .. 'z'])
    <*> ('$' :: ['a' .. 'z'])
    <*> ['a' .. 'z']
    |> List.filter (fun (a,b,_) -> not (a <> '$' && b = '$'))
    |> List.fold (flip add) m

let constructModel (words : Word list) =
    let model = Map.empty

    words
    |> List.map addStartSymbols
    |> List.collect constructTrigrams
    |> List.fold (flip add) model
    |> addSmoothing

[<EntryPoint>]
let main argv = 
    let word = "muradin"
    word
    |> addStartSymbols
    |> constructTrigrams
    |> List.fold (flip add) Map.empty
    |> addSmoothing
    |> printfn "%A"
    0 // return an integer exit code

