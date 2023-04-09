
module Language = Map.Make(String)

type rule =
    | Empty
    | Ref of string
    | Eps
    | Str of string
    | Rep of rule
    | Or of rule * rule
    | Seq of rule * rule

let is_nullable language (self: (rule -> bool) Yfix.self) = function
    | Empty -> false
    | Ref ref -> self (Language.find ref language) ~fix_here:true
    | Eps -> true
    | Str _ -> false
    | Rep _ -> true
    | Or (l0, l1) -> (self l0) || (self l1)
    | Seq (l0, l1) -> (self l0) && (self l1)
