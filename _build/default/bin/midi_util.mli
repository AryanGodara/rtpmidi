module Syntax : sig
  val ( let+ ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
  val ( let* ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  val ( let*! ) : 'a 'b 'e. ('a, 'e) result -> ('a -> 'b) -> 'b
  val ( >>| ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
  val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
end
