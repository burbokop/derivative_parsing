

type 'q self = (?fix_here:bool -> 'q)


val y: (('a -> 'b) self -> 'a -> 'b) -> 'b -> ('a -> 'b) self
