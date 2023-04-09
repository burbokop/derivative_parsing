
 
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

module YMem: Y.Y = YMemImpl
