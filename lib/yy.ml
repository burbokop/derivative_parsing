
(** Simple Y Combinator implementation *)
module YYImpl = struct  
  let y f =
    let rec g x = f g x 
    in g
end

module YY: Y.Y = YYImpl
