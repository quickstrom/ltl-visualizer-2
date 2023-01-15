open! Core
open Ltl

module Model : sig
  type t
         (* [@@deriving sexp] *)

  include Incr_dom.App_intf.Model with type t := t
end

module Action : sig
  type t [@@deriving sexp]
end

val initial_model_exn : Formula.formula list -> Model.t

include
  Incr_dom.App_intf.S
  with module Model := Model
   and module Action := Action
   and type State.t = unit
