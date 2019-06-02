module type EvalSig = sig
  val eval : Types.env -> Types.exp -> Types.value
  val bind_pat : Types.pat -> Types.value -> Types.env option
end

module EvalCheck : EvalSig = Eval
