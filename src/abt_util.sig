signature ABT_UTIL =
sig
  include ABT

  val `` : Variable.t -> t
  val \\ : Variable.t * t -> t
  val $$ : Operator.t * t vector -> t

  val freeVariables : t -> Variable.t list
  val operators : t -> Operator.t list
  val hasFree : t * Variable.t -> bool

  val subst : t -> Variable.t -> t -> t
  val substOperator : (t Vector.vector -> t) -> Operator.t -> t -> t
  val toString : t -> string
  val toStringOpen : (t -> string) -> t -> string

  val unbind : t -> Variable.t * t

  val subst1 : (* binding *) t -> t -> t
  val // : (* binding *) t * t -> t

  exception ExpectedBinding of t

end
