
type rule =
    | Empty
    | Ref of string
    | Eps
    | Str of string
    | Rep of rule
    | Or of rule * rule
    | Seq of rule * rule

module Language: Map.S with type key = string

val is_nullable: (rule Language.t) -> (rule -> bool) Yfix.self -> rule -> bool

