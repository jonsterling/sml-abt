functor Abt
  (structure Operator : OPERATOR
   structure Variable : VARIABLE) : ABT =
struct
  structure Operator = Operator
  structure Variable = Variable

  infix 5 \
  infix 5 $

  datatype t =
      FREE of Variable.t
    | BOUND of int
    | ABS of Variable.t * t
    | APP of Operator.t * t vector

  datatype 'a view =
      ` of Variable.t
    | \ of Variable.t * 'a
    | $ of Operator.t * 'a vector

  fun map f (` v) = ` v
    | map f (v \ e') = v \ f e'
    | map f (p $ es) = p $ Vector.map f es

  fun shiftVar v n (FREE v') = if Variable.eq (v, v') then BOUND n else (FREE v')
    | shiftVar v n (BOUND m) = BOUND m
    | shiftVar v n (ABS (x, e')) = ABS (x, shiftVar v (n + 1) e')
    | shiftVar v n (APP (p, es)) = APP (p, Vector.map (shiftVar v n) es)

  fun matchArity (0, ABS _) = false
    | matchArity (0, _) = true
    | matchArity (n, ABS (_, e')) = matchArity (n - 1, e')
    | matchArity (n, _) = false

  exception Malformed of string

  fun doApp (oper, es) =
    if VectorPair.pairAllEq matchArity (Operator.arity oper, es) then
      APP (oper, es)
    else
      raise Malformed ("Bad arity: " ^ Operator.toString oper)

  fun into (` v) = FREE v
    | into (v \ e') = ABS (v, shiftVar v 0 e')
    | into (p $ es) = doApp (p, es)

  fun addVar v n (FREE v') = FREE v'
    | addVar v n (BOUND m) = if m = n then FREE v else BOUND m
    | addVar v n (ABS (x, e)) = ABS (x, addVar v (n + 1) e)
    | addVar v n (APP (p, es)) = APP (p, Vector.map (addVar v n) es)

  fun out e =
    case e of
      FREE v => ` v
    | BOUND n => raise Malformed "bound variable occured in out"
    | ABS (x, e') =>
      let
        val v = Variable.clone x
      in
        v \ addVar v 0 e'
      end
    | APP (p, es) => p $ es

  fun eq (FREE v1, FREE v2) = Variable.eq (v1, v2)
    | eq (BOUND m, BOUND n) = m = n
    | eq (ABS (_, e1), ABS (_, e2)) = eq (e1, e2)
    | eq (APP (p1, es1), APP (p2, es2)) =
        Operator.eq (p1, p2) andalso VectorPair.pairAllEq eq (es1, es2)
    | eq (_, _) = false

end
