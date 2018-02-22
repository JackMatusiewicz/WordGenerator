namespace WordGenerator
open System

module ListExtension =

    let (<?>) = List.map

    let listApply (fs : ('a -> 'b) list) (xs : 'a list) =
        let rec calc (acc : 'b list list) fs =
            match fs with
            | [] ->
                acc
                |> List.rev
                |> List.concat
            | (h::t) ->
                calc ((h <?> xs) :: acc) t
        calc [] fs
    let (<&>) = listApply