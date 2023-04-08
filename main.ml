(** No-rec memoization *)
let memo f =
  let h = Hashtbl.create 11 in
  fun x ->
    try Hashtbl.find h x
    with Not_found ->
      let y = f x in
      Hashtbl.add h x y;
      y


(** Y Combinator signature *)
module type Y = sig
  val y: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
end

(** Simple Y Combinator implementation *)
module YYImpl = struct  
  let y f =
    let rec g x = f g x 
    in g
end

module YY: Y = YYImpl

(** Memoization Y Combinator implementation *)
module YMemImpl = struct
  let y f =
    let h = Hashtbl.create 16 in
    let rec g x =
      try Hashtbl.find h x
      with Not_found ->
        let r = f g x in
        Hashtbl.add h x r;
        r
    in
    g
end

module YMem: Y = YMemImpl

(** Fixed-Point Y Combinator implementation *)
module YFix = struct
  type 'q self = (?fix_here:bool -> 'q)

  let y f assumption =
    let counter = ref 0 in
    let ass = ref assumption in
    let rec g ?(fix_here = false) x =
      let first_call = !counter == 0 in counter := (!counter + 1);
      let result = if fix_here
        then !ass
        else f g x
      in
        if first_call && !ass != result
          then (ass := result; counter := 0; g x)
          else result
    in
      g
end

let y f =
  let rec g x = f g x 
  in g

let rec fix f x = f (fix f) x

let fact self = function
  | 0 -> 1
  | x -> x * self (x-1)

let fact_with_fix_point (self: (int -> int) YFix.self) = function
  | 0 -> 1
  | x -> x * self (x-1)

type lang =
  | Empty
  | Ref of string
  | Eps
  | Str of string
  | Rep of lang
  | Or of lang * lang
  | Seq of lang * lang

module StrMap = Map.Make(String);;

let rec is_nullable rules (self: (lang -> bool) YFix.self) = function
  | Empty -> Printf.printf "Empty\n"; false
  | Ref ref -> Printf.printf "Ref(%s)\n" ref; self (StrMap.find ref rules) ~fix_here:true
  | Eps -> Printf.printf "Eps\n"; true
  | Str _ -> Printf.printf "Str\n"; false
  | Rep _ -> Printf.printf "Rep\n"; true
  | Or (l0, l1) -> Printf.printf "Or(??, ??)\n"; (self l0) || (self l1)
  | Seq (l0, l1) -> Printf.printf "Seq(??, ??)\n"; (self l0) && (self l1)

let _ = (fix fact) 5
let _ = (y fact) 5

let fact_yy_res = (YY.y fact) 5
let fact_ymem_res = (YMem.y fact) 5
let fact_fix_res = (YFix.y fact_with_fix_point 0) 5

let language = StrMap.(
  add "L" (Or(Seq(Ref("L"), Str("x")), Eps)) 
  empty)

let delta = (is_nullable language)

let _ = print_endline "--------------------------------------------------"

let delta_res = (YFix.y delta false) (StrMap.find "L" language)
