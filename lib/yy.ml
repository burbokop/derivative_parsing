
(** Simple Y Combinator implementation *)
module YYImpl = struct  
  let y f =
    let rec g x = f g x 
    in g


  let y_limited f default limit = 
    let rec g lvl x = 
      if lvl > limit
        then let _ = Printf.eprintf "warning: recursion limited due to reach max level %d\n%!" limit in default
        else f (g (lvl + 1)) x
    in g 0
end

module YY: Y.Y = YYImpl
