namespace WordGenerator

module Tuple =

    let map (f : 'a -> 'b) ((c,a) : 'c*'a) =
        c, (f a)