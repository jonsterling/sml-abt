signature VECTOR_PAIR =
sig
  type 'a vector

  val zip : 'a vector * 'b vector -> ('a * 'b) vector

  val pairAll   : ('a * 'b -> bool) -> 'a vector * 'b vector -> bool
  val pairAllEq : ('a * 'b -> bool) -> 'a vector * 'b vector -> bool
end
