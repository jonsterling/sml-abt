signature OPERATOR =
sig
  type t

  val eq : t * t -> bool
  val arity : t -> Arity.t
  val toString : t -> string

end
