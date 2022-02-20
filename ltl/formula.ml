type un_op = Not | Next | Always | Eventually
[@@deriving eq, show, variants]

type bin_op = And | Or | Implies | Until [@@deriving eq, show, variants]

type formula =
  | Top
  | Bottom
  | Atomic of char
  | Un_op of un_op * formula
  | Bin_op of bin_op * formula * formula
[@@deriving eq, show, variants]

module Syntax = struct
  let ( ! ) p = Un_op (Not, p)

  let next p = Un_op (Next, p)

  let always p = Un_op (Always, p)

  let eventually p = Un_op (Eventually, p)

  let ( || ) p q = Bin_op (Or, p, q)

  let ( && ) p q = Bin_op (And, p, q)

  let ( => ) p q = Bin_op (Implies, p, q)

  let until p q = Bin_op (Until, p, q)
end
