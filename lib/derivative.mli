
type rule =
    | Empty
    | Ref of string
    | Eps
    | Char of char
    | Rep of rule lazy_t
    | Or of rule lazy_t * rule lazy_t
    | Seq of rule lazy_t * rule lazy_t

type lrule = rule lazy_t

val rule_pp: Format.formatter -> rule -> unit
val rule_ppl: Format.formatter -> lrule -> unit
val rule_ppy: Format.formatter -> (lrule -> unit) -> lrule -> unit

(** Char sequance of string  *)
val seq_of_string: string -> lrule

(** Char Concatenation *)
val ( #.> ): char -> lrule -> lrule

(** Concatenation (Seq) *)
val ( #..> ): lrule -> lrule -> lrule

(** Union (Or) *)
val ( <||> ): lrule -> lrule -> lrule

(** Char Concatenation *)
val ( #.>^ ): char -> lrule -> rule

(** Concatenation (Seq) *)
val ( #..>^ ): lrule -> lrule -> rule

(** Union (Or) *)
val ( <||>^ ): lrule -> lrule -> rule



module Language: Map.S with type key = string

val is_nullable: (lrule Language.t) -> (lrule -> bool) Yfix.self -> lrule -> bool

(** 
    Derivative of rule 
    arg delta - Y combinated is nullable predicate
    arg language - rules map where key=string value=rule
    arg c - symbol
    *)
val derivative: (lrule -> bool) -> (lrule Language.t) -> char -> (lrule -> lrule) -> lrule -> lrule

(** Make rule as simple as possible with no changing of meaning *)
val simplify: lrule -> lrule

val eq: lrule -> lrule -> bool