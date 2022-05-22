open! Core_kernel
open Incr_dom

(** The [Model] represents the full state of the application. The module has
    methods for updating the model as well, which will be used when applying
    actions, in the [apply_action] function below. *)
module Model = struct
  type t = {formulas: Ltl.Formula.formula list} [@@deriving fields, equal]

  let cutoff t1 t2 = equal t1 t2

  let add_new_formula t f = {formulas= List.append t.formulas [f]}

  let remove_formula t i =
    { formulas=
        (let before, after = List.split_n t.formulas (i + 1) in
         List.drop_last_exn before @ after ) }
end

(** The [Action] type represents transactions whose purpose is to update the
    model, and maybe kick off other imperative actions. Note that the design
    of the action type is important! In particular, you could imagine having
    [update] set an absolute value for the counter, rather than a diff. This,
    however, would work quite differently if you quickly clicked the button a
    few times in a row, before a refresh could occur. Always be aware when
    designing the action type that actions will be interpreted at some
    unpredictable time after they're initiated. *)
module Action = struct
  type t =
    | New_formula of {formula: string}
    | Remove_formula of {index: int}
    | Add_state
    | Toggle_state_atomic of {index: int; name: char}
  [@@deriving sexp]
end

(** The state is for holding real imperative state, like RPC connections. We
    have none of those here, so we make it trivial. *)
module State = struct
  type t = unit
end

let parse str = Ltl.Parser.f Ltl.Lexer.f (Lexing.from_string str)

let apply_action model action _ ~schedule_action:_ =
  match (action : Action.t) with
  | Action.New_formula {formula} ->
      Model.add_new_formula model (parse formula)
  | Action.Remove_formula {index} -> Model.remove_formula model index
  (* TODO: implement *)
  | Action.Add_state -> model
  | Action.Toggle_state_atomic _ -> model

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let view (m : Model.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom in
  let add_new_formula_button =
    let on_add_new_click =
      Attr.on_click (fun _ev ->
          inject (Action.New_formula {formula= "always (true || next false)"}) )
    in
    Node.div [] [Node.button [on_add_new_click] [Node.text "Add formula"]]
  in
  let remove_formula_button txt ~index =
    let on_click _ev = inject (Action.Remove_formula {index}) in
    Node.button [Attr.on_click on_click] [Node.text txt]
  in
  let elements =
    List.mapi m.formulas ~f:(fun index formula ->
        Node.div []
          [ remove_formula_button "Remove" ~index
          ; Node.text (Ltl.Printer.print_formula formula) ] )
  in
  Node.body [] (add_new_formula_button :: Node.hr [] :: elements)

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  (* Here we use Incremental in a trivial way, just having everything
     recompute every time the model changes. That approach is actually just
     fine for most small applications. Only use Incremental where you need
     to! *)
  let%map model = model in
  let apply_action = apply_action model in
  let view = view model ~inject in
  Component.create ~apply_action model view

let initial_model_exn formulas = {Model.formulas}
