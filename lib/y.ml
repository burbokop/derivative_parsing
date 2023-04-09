
(** Y Combinator signature *)
module type Y = sig
   val y: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
end
