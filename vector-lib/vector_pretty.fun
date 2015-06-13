functor VectorPretty (Vector : VECTOR) : VECTOR_PRETTY =
struct
  open Vector

  fun toString f v =
    "("
      ^ foldri (fn (i, s1, s2) => if i = (Vector.length v - 1) then s1 else s1 ^ "; " ^ s2) "" (map f v)
      ^ ")"
end

structure VectorPretty = VectorPretty (Vector)
