
module Language = Map.Make(String)

type rule =
    | Empty
    | Ref of string
    | Eps
    | Char of char
    | Rep of rule
    | Or of rule * rule
    | Seq of rule * rule

let rec rule_pp f = function
    | Empty -> Format.fprintf f "∅"
    | Ref ref -> Format.fprintf f "&%s" ref
    | Eps -> Format.fprintf f "Ɛ"
    | Char c -> Format.fprintf f "%c" c
    | Rep l0 -> Format.fprintf f "(%a)*" rule_pp l0
    | Or (l0, l1) -> Format.fprintf f "(%a ∪ %a)" rule_pp l0 rule_pp l1
    | Seq (l0, l1) -> Format.fprintf f "(%a.%a)" rule_pp l0 rule_pp l1

let is_nullable language (self: (rule -> bool) Yfix.self) = function
    | Empty -> false
    | Ref ref -> self (Language.find ref language) ~fix_here:true
    | Eps -> true
    | Char _ -> false
    | Rep _ -> true
    | Or (l0, l1) -> (self l0) || (self l1)
    | Seq (l0, l1) -> (self l0) && (self l1)


(** Char Concatenation *)
let ( #.> ) h t = Seq(Char(h), t)

let seq_of_string str = 
    let lst = (List.init (String.length str) (String.get str)) in
    let rec iter = function
        | [] -> Eps
        | h :: t -> Seq(Char(h), (iter t))
    in 
        iter lst


(** Concatenation *)
let ( #..> ) l0 l1 = Seq(l0, l1)

(** Union *)
let ( <||> ) l0 l1 = Or(l0, l1)

let derivative delta language c self l = match l with
    | Empty -> Empty
    | Ref ref -> self (Language.find ref language)
    | Eps -> Empty
    | Char x -> if c == x then Eps else Empty
    | Rep l0 -> (self l0) #..> l
    | Or (l0, l1) -> (self l0) <||> (self l1)
    | Seq (l0, l1) -> if delta l0
        then (self l1) <||> ((self l0) #..> l1)
        else (self l0) #..> l1


let rec simplify = function
    | Rep (Empty) -> Empty
    | Rep (Eps) -> Eps
    | Or  (l0, l1) when simplify l0 == Empty -> simplify l1
    | Or  (l0, l1) when simplify l1 == Empty -> simplify l0
    | Or  (l0, l1) -> (simplify l0) <||> (simplify l1)
    | Seq (Empty, _) -> Empty
    | Seq (_, Empty) -> Empty
    | Seq (Eps, l1) -> simplify l1
    | Seq (l0, Eps) -> simplify l0
    | Seq (l0, l1) -> (simplify l0) #..> (simplify l1)
    | other -> other