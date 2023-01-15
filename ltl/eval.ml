exception EmptyTrace

open Core

type strength = True | False

type value = Residual of residual | Pure of bool

and residual =
  | Conjunction of residual * residual
  | Disjunction of residual * residual
  | Next of Formula.formula * value

let rec negate_residual = function
  | Conjunction (p, q) -> Disjunction (negate_residual p, negate_residual q)
  | Disjunction (p, q) -> Conjunction (negate_residual p, negate_residual q)
  | Next (f, r) -> Next (Formula.Un_op (Formula.Not, f), negate_value r)

and negate_value = function
  | Residual r -> Residual (negate_residual r)
  | Pure v -> Pure (not v)

let eval_and p q =
  match (p, q) with
  | Pure false, _ -> Pure false
  | Pure true, _ -> q
  | Residual _, Pure false -> Pure false
  | Residual r1, Pure true -> Residual r1
  | Residual r1, Residual r2 -> Residual (Conjunction (r1, r2))

let eval_or p q =
  match (p, q) with
  | Pure true, _ -> Pure true
  | Pure false, _ -> q
  | Residual _, Pure true -> Pure true
  | Residual r1, Pure false -> Residual r1
  | Residual r1, Residual r2 -> Residual (Disjunction (r1, r2))

let rec eval f (state: Trace.State.t) =
  Formula.Syntax.(
    match f with
    | Formula.Top -> Pure true
    | Formula.Bottom -> Pure true
    | Formula.Atomic c -> Pure (Trace.State.mem c state)
    | Formula.Un_op (op, p) -> (
      match op with
      | Formula.Not -> negate_value (eval p state)
      | Formula.Next -> Residual (Next (p, eval p state))
      | Formula.Always -> (
        match eval p state with
        | Pure false -> Pure false
        | Pure true -> Residual (Next (always p, Pure true))
        | Residual r ->
            Residual (Conjunction (r, Next (always p, Residual r))) )
      | Formula.Eventually -> (
        match eval p state with
        | Pure true -> Pure true
        | Pure false -> Residual (Next (eventually p, Pure false))
        | Residual r ->
            Residual (Disjunction (r, Next (eventually p, Residual r))) ) )
    | Formula.Bin_op (op, p, q) -> (
      match op with
      | Formula.Or -> eval_or (eval p state) (eval q state)
      | Formula.And -> eval_and (eval p state) (eval q state)
      | Formula.Implies -> eval (!p || q) state
      | Formula.Until -> (
          let cont = Next (until p q, eval q state) in
          match (eval p state, eval q state) with
          | _, Pure true -> Pure true
          | Pure true, Pure false -> Residual cont
          | Pure false, q' -> q'
          | Residual r, Pure false -> Residual (Conjunction (r, cont))
          | Residual pr, Residual qr ->
              Residual (Disjunction (qr, Conjunction (pr, cont)))
          | Pure true, Residual qr -> Residual (Disjunction (qr, cont)) ) ))

module EvalTrace = struct
  let rec step r state =
    match r with
    | Conjunction (r1, r2) -> eval_and (step r1 state) (step r2 state)
    | Disjunction (r1, r2) -> eval_or (step r1 state) (step r2 state)
    | Next (f, _) -> eval f state

  let rec stop = function
    | Conjunction (r1, r2) -> stop r1 && stop r2
    | Disjunction (r1, r2) -> stop r1 || stop r2
    | Next (_, Pure b) -> b
    | Next (_, Residual r) -> stop r

  let loop_last = function Pure r -> r | Residual r -> stop r

  let rec step_states value trace =
    match trace with
    | [] -> raise EmptyTrace
    | [last] -> (
      match value with Pure r -> r | Residual r -> loop_last (step r last) )
    | current :: rest -> (
      match value with
      | Pure r -> r
      | Residual r -> step_states (step r current) rest )

  let eval_trace f (t: Trace.trace) =
    match t with
    | [] -> raise EmptyTrace
    | [last] -> loop_last (eval f last)
    | first :: rest -> step_states (eval f first) rest

  let rec eval_all f (t : Trace.trace) =
    match t with [] -> [] | _ :: rest -> eval_trace f t :: eval_all f rest
end
