namespace WordGenerator
open System

type Word = string
type Occurrences = Map<char, int>
type Prefix = {First : char; Second : char}
type Trigram = char*char*char
type TrigramStore = Map<Prefix, Occurrences>