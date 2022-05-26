open! Core_kernel
open Incr_dom
open Vdom

exception ParseError of {line: int; column: int; message: string}

let parse str =
  let buf = Lexing.from_string str in
  try Ltl.Parser.f Ltl.Lexer.f buf with
  | Ltl.Parser.Error ->
      let pos = buf.lex_curr_p in
      raise
        (ParseError
           {line= pos.pos_lnum; column= pos.pos_cnum; message= "Parser error"}
        )
  | Ltl.Lexer.SyntaxError msg ->
      let pos = buf.lex_curr_p in
      raise
        (ParseError {line= pos.pos_lnum; column= pos.pos_cnum; message= msg})

(** The [Model] represents the full state of the application. The module has
    methods for updating the model as well, which will be used when applying
    actions, in the [apply_action] function below. *)
module Model = struct
  type t =
    { formula_error: string option
    ; pending: string
    ; formulas: Ltl.Formula.formula list }
  [@@deriving fields, equal]

  let cutoff t1 t2 = equal t1 t2

  let set_pending str t = {t with pending= str}

  let add_new_formula t =
    try {t with formulas= List.append t.formulas [parse t.pending]}
    with ParseError {line; column; message} ->
      { t with
        formula_error= Some (Printf.sprintf "%d:%d: %s" line column message)
      }

  let remove_formula i t =
    { t with
      formulas=
        (let before, after = List.split_n t.formulas (i + 1) in
         List.drop_last_exn before @ after ) }

  let clear_error t = {t with formula_error= None}
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
    | Add_formula
    | Set_pending of {formula: string}
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

let apply_action model action _ ~schedule_action:_ =
  match (action : Action.t) with
  | Action.Add_formula -> Model.add_new_formula model
  | Action.Set_pending {formula} ->
      model |> Model.set_pending formula |> Model.clear_error
  | Action.Remove_formula {index} ->
      model |> Model.remove_formula index |> Model.clear_error
  (* TODO: implement *)
  | Action.Add_state -> model
  | Action.Toggle_state_atomic _ -> model

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let delimiter t = Node.span [Attr.class_ "delimiter"] [Node.text t]

let literal t = Node.span [Attr.class_ "literal"] [Node.text t]

let unary_operator t =
  Node.span [Attr.classes ["unary"; "operator"]] [Node.text t]

let binary_operator t =
  Node.span [Attr.classes ["binary"; "operator"]] [Node.text t]

let in_parens param s =
  if param then
    Node.span [Attr.class_ "parens"] [delimiter "("; s; delimiter ")"]
  else s

let rec print_formula_html ?(param = false) f =
  let open Ltl.Formula in
  match f with
  | Top -> literal "true"
  | Bottom -> literal "false"
  | Atomic a -> literal (String.make 1 a)
  | Un_op (op, f') ->
      let op_str =
        match op with
        | Not -> "not"
        | Next -> "next"
        | Always -> "always"
        | Eventually -> "eventually"
      in
      in_parens param
        (Node.span []
           [ unary_operator op_str
           ; Node.text " "
           ; print_formula_html f' ~param:true ] )
  | Bin_op (op, f1, f2) ->
      let op_str =
        match op with
        | And -> "&&"
        | Or -> "||"
        | Implies -> "->"
        | Until -> "until"
      in
      in_parens param
        (Node.span []
           [ print_formula_html f1 ~param:true
           ; Node.text " "
           ; binary_operator op_str
           ; Node.text " "
           ; print_formula_html f2 ~param:true ] )

let view (m : Model.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom in
  let add_new_formula_button =
    let on_add_new_click =
      Attr.on_click (fun _ev -> inject Action.Add_formula)
    in
    Node.div [] [Node.button [on_add_new_click] [Node.text "Add formula"]]
  in
  let pending_formula_input =
    Node.textarea
      [ Attr.on_input (fun _ formula ->
            inject (Action.Set_pending {formula}) ) ]
      []
  in
  let remove_formula_button txt ~index =
    let on_click _ev = inject (Action.Remove_formula {index}) in
    Node.button [Attr.on_click on_click] [Node.text txt]
  in
  let messages =
    Node.div []
      (match m.formula_error with Some msg -> [Node.text msg] | None -> [])
  in
  let elements =
    Node.table []
      ( Node.tr [] [Node.th [] [Node.text "Formula"]; Node.th [] []]
      :: List.mapi m.formulas ~f:(fun index formula ->
             Node.tr []
               [ Node.td [Attr.class_ "formula"]
                   [Node.code [] [print_formula_html formula]]
               ; Node.td [] [remove_formula_button "Remove" ~index] ] ) )
  in
  Node.body []
    [pending_formula_input; add_new_formula_button; messages; elements]

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

let initial_model_exn formulas =
  {formula_error= None; pending= ""; Model.formulas}
