
(** Fixed-Point Y Combinator implementation *)
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
