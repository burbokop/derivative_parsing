
module Language = Map.Make(String)

type rule =
    | Empty
    | Ref of string
    | Eps
    | Char of char
    | Rep of rule lazy_t
    | Or of rule lazy_t * rule lazy_t
    | Seq of rule lazy_t * rule lazy_t

type lrule = rule lazy_t

let rule_ppy f (self: (lrule -> unit)) r = 
    let self_pp = (fun _ x -> self x) in 
    match Lazy.force r with
        | Empty -> Format.fprintf f "∅"
        | Ref ref -> Format.fprintf f "&%s" ref
        | Eps -> Format.fprintf f "Ɛ"
        | Char c -> Format.fprintf f "%c" c
        | Rep l0 -> Format.fprintf f "(%a)*" self_pp l0
        | Or (l0, l1) -> Format.fprintf f "(%a ∪ %a)" self_pp l0 self_pp l1
        | Seq (l0, l1) -> Format.fprintf f "(%a.%a)" self_pp l0 self_pp l1

let rule_ppl f r = (Yy.YYImpl.y_limited (rule_ppy f) () 30) r

let rule_pp f r = (Yy.YYImpl.y_limited (rule_ppy f) () 30) (lazy(r))

let is_nullable language (self: (lrule -> bool) Yfix.self) r = match Lazy.force r with
    | Empty -> false
    | Ref ref -> self (Language.find ref language) ~fix_here:true
    | Eps -> true
    | Char _ -> false
    | Rep _ -> true
    | Or (l0, l1) -> (self l0) || (self l1)
    | Seq (l0, l1) -> (self l0) && (self l1)


(** Char Concatenation *)
let ( #.> ) h t = lazy (Seq(lazy (Char(h)), t))

(** Concatenation *)
let ( #..> ) l0 l1 = lazy (Seq(l0, l1))

(** Union *)
let ( <||> ) l0 l1 = lazy (Or(l0, l1))

let ( #.>^ ) h t = Seq(lazy (Char(h)), t)

(** Concatenation (Seq) *)
let ( #..>^ ) l0 l1 = Seq(l0, l1)

(** Union (Or) *)
let ( <||>^ ) l0 l1 = Or(l0, l1)


let seq_of_string str = 
    let lst = (List.init (String.length str) (String.get str)) in
    let rec iter = function
        | [] -> Eps
        | h :: t -> Seq(lazy (Char(h)), lazy (iter t))
    in 
        lazy (iter lst)



let derivative delta language c self l = match (Lazy.force l) with
    | Empty -> lazy Empty
    | Ref ref -> self (Language.find ref language)
    | Eps -> lazy Empty
    | Char x -> lazy (if c == x then Eps else Empty)
    | Rep l0 -> lazy ((self l0) #..>^ l)
    | Or (l0, l1) -> lazy ((self l0) <||>^ (self l1))
    | Seq (l0, l1) -> if delta l0
        then lazy ((self l1) <||>^ lazy ((self l0) #..>^ l1))
        else lazy ((self l0) #..>^ l1)


let rec simplify x = match (Lazy.force x) with
    | Rep (lazy Empty) -> lazy Empty
    | Rep (lazy Eps) -> lazy Eps
    | Or  (l0, l1) when simplify l0 == lazy Empty -> simplify l1
    | Or  (l0, l1) when simplify l1 == lazy Empty -> simplify l0
    | Or  (l0, l1) -> lazy ((simplify l0) <||>^ (simplify l1))
    | Seq (lazy Empty, _) -> lazy Empty
    | Seq (_, lazy Empty) -> lazy Empty
    | Seq (lazy Eps, l1) -> simplify l1
    | Seq (l0, lazy Eps) -> simplify l0
    | Seq (l0, l1) -> lazy ((simplify l0) #..>^ (simplify l1))
    | other -> lazy other


let rec eq a b = match (Lazy.force a), (Lazy.force b) with 
    | Empty, Empty -> true
    | Ref r0, Ref r1 -> r0 = r1
    | Eps, Eps -> true
    | Char c0, Char c1 -> c0 == c1
    | Rep l0, Rep l1 -> eq l0 l1
    | Or (l0, l1), Or (r0, r1) -> (eq l0 r0) && (eq l1 r1)
    | Seq (l0, l1), Seq (r0, r1) -> (eq l0 r0) && (eq l1 r1)
    | _, _ -> false
