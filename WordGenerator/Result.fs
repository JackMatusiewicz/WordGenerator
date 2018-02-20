﻿namespace WordGenerator

type Result<'failure, 'success> =
    | Failure of 'failure
    | Success of 'success

//[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Result =

    //TODO - move this into its own List extension file.
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

    let orDefault (v : 'a) (x : Result<'f, 'a>) =
        match x with
        | Failure f -> v
        | Success x -> x

    let lift a = Success a

    let map (f : 'a -> 'b) (a : Result<'f,'a>) : Result<'f, 'b> =
        match a with
        | Failure f -> Failure f
        | Success s -> f s |> Success

    let apply (f : Result<'f, 'a -> 'b>) (a : Result<'f, 'a>) : Result<'f, 'b> =
        match f with
        | Failure f -> Failure f
        | Success s -> map s a

    let bind (a : Result<'f, 'a>) (f : 'a -> Result<'f, 'b>) : Result<'f, 'b> =
        match a with
        | Failure f -> Failure f
        | Success s -> f s

    let iter (f : 'a -> unit) (a : Result<'f, 'a>) =
        match a with
        | Success x -> f x
        | _ -> ()

    let compose (f : 'a -> Result<'f, 'b>) (g : 'b -> Result<'f, 'c>) =
        fun a -> bind (f a) g

    let (<!>) = map
    let (<*>) = apply
    let (>>=) = bind
    let (>=>) = compose

    let prepend x xs = x::xs

    let rec traverse (f : 'a -> Result<'f, 'b>) (xs : 'a list) : Result<'f, 'b list> =
        match xs with
        | [] -> lift []
        | (h::t) -> prepend <!> (f h) <*> (traverse f t)

    type ResultBuilder () =
        member __.Return a = Success a
        member __.ReturnFrom a = a
        member __.Bind (f,a) = bind a f

    let result = ResultBuilder ()