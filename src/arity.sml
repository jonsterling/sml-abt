structure Arity =
struct
  type t = int vector
  val toString = VectorPretty.toString Int.toString
end
