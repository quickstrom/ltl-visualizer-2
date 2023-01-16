let rec print_formula ?(param = false) f =
  let in_parens s = if param then "(" ^ s ^ ")" else s in
  match f with
  | Formula.Top -> "true"
  | Formula.Bottom -> "false"
  | Formula.Atomic a -> String.make 1 a
  | Formula.Un_op (op, f') ->
      let op_str =
        match op with
        | Formula.Not -> "not"
        | Formula.Next -> "next"
        | Formula.Always -> "always"
        | Formula.Eventually -> "eventually"
      in
      in_parens (op_str ^ " " ^ print_formula f' ~param:true)
  | Formula.Bin_op (op, f1, f2) ->
      let op_str =
        match op with
        | Formula.And -> "&&"
        | Formula.Or -> "||"
        | Formula.Implies -> "=>"
        | Formula.Until -> "until"
      in
      in_parens
        (print_formula f1 ~param:true ^ " " ^ op_str ^ " " ^ print_formula f2 ~param:true)


let rec print_value = function
    | Eval.Residual(r) -> print_residual(r)
    | Eval.Pure(b) -> string_of_bool(b)

and print_residual = function
  | Conjunction(r1, r2) -> print_residual(r1) ^ " && " ^ print_residual(r2)
  | Disjunction(r1, r2) -> print_residual(r1) ^ " || " ^ print_residual(r2)
  | Next(formula, value) ->
    "next(" ^ print_formula(formula) ^ ", " ^ print_value(value) ^ ")"
