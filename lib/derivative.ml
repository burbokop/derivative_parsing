
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
    | Empty -> Printf.printf "Empty\n"; false
    | Ref ref -> Printf.printf "Ref(%s)\n" ref; self (Language.find ref language) ~fix_here:true
    | Eps -> Printf.printf "Eps\n"; true
    | Str _ -> Printf.printf "Str\n"; false
    | Rep _ -> Printf.printf "Rep\n"; true
    | Or (l0, l1) -> Printf.printf "Or(??, ??)\n"; (self l0) || (self l1)
    | Seq (l0, l1) -> Printf.printf "Seq(??, ??)\n"; (self l0) && (self l1)
