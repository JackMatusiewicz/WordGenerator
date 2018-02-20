namespace WordGenerator

type Result<'failure, 'success> =
    | Failure of 'failure
    | Success of 'success

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Result =

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

    let compose (f : 'a -> Result<'f, 'b>) (g : 'b -> Result<'f, 'c>) =
        fun a -> bind (f a) g

    let (<!>) = map
    let (<*>) = apply
    let (>>=) = bind
    let (>=>) = compose
