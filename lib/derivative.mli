
type rule =
    | Empty
    | Ref of string
    | Eps
    | Char of char
    | Rep of rule
    | Or of rule * rule
    | Seq of rule * rule

val rule_pp : Format.formatter -> rule -> unit


(** Char sequance of string  *)
val seq_of_string: string -> rule

(** Char Concatenation *)
val ( #.> ): char -> rule -> rule

(** Concatenation (Seq) *)
val ( #..> ): rule -> rule -> rule

(** Union (Or) *)
val ( <||> ): rule -> rule -> rule

module Language: Map.S with type key = string

val is_nullable: (rule Language.t) -> (rule -> bool) Yfix.self -> rule -> bool

(** 
    Derivative of rule 
    arg delta - Y combinated is nullable predicate
    arg language - rules map where key=string value=rule
    arg c - symbol
    *)
val derivative: (rule -> bool) -> (rule Language.t) -> char -> (rule -> rule) -> rule -> rule

(** Make rule as simple as possible with no changing of meaning *)
val simplify: rule -> rule
