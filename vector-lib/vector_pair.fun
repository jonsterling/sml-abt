functor VectorPair (Vector : VECTOR) : VECTOR_PAIR =
struct
  open Vector

  fun zip (v1, v2) =
    let
      val len = Int.min (Vector.length v1, Vector.length v2)
    in
      tabulate (len, fn n => (Vector.sub (v1, n), Vector.sub (v2, n)))
    end

  fun pairAll f =
    foldl (fn (x,b) => b andalso f x) true o zip

  fun pairAllEq f (v1, v2) =
    Vector.length v1 = Vector.length v2 andalso
    foldl (fn (x,b) => b andalso f x) true (zip (v1, v2))
end

structure VectorPair = VectorPair (Vector)
