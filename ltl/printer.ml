let rec print ?(param = false) f =
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
      in_parens (op_str ^ " " ^ print f' ~param:true)
  | Formula.Bin_op (op, f1, f2) ->
      let op_str =
        match op with
        | Formula.And -> "&&"
        | Formula.Or -> "||"
        | Formula.Implies -> "->"
        | Formula.Until -> "until"
      in
      in_parens
        (print f1 ~param:true ^ " " ^ op_str ^ " " ^ print f2 ~param:true)
