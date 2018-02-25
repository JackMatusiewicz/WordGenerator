namespace WordGenerator

module Tuple =

    let bimap (f : 'a -> 'b) (g : 'c -> 'd) ((a,b) : 'a*'c) =
        (f a),(g b)

    let leftMap (f : 'a -> 'b) =
        bimap f id

    let rightMap (f : 'a -> 'b) =
        bimap id f