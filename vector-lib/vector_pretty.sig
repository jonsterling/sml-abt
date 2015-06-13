signature VECTOR_PRETTY =
sig
  type 'a vector
  val toString : ('a -> string) -> 'a vector -> string
end

