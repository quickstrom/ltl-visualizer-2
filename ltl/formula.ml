type un_op = Not | Next | Always | Eventually [@@deriving eq, show, variants]

type bin_op = And | Or | Implies | Until [@@deriving eq, show, variants]

type formula =
  | Top
  | Bottom
  | Atomic of char
  | Un_op of un_op * formula
  | Bin_op of bin_op * formula * formula
[@@deriving eq, show, variants]
