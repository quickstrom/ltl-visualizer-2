open! Core
open Incr_dom
open Vdom

exception PositionedParseError of {line: int; column: int; message: string}

let parse str =
  let buf = Lexing.from_string str in
  try Ltl.Parser.f Ltl.Lexer.f buf with
  | Ltl.Parser.Error ->
      let pos = buf.lex_start_p in
      raise
        (PositionedParseError
           { line= pos.pos_lnum
           ; column= pos.pos_cnum
           ; message= "Invalid syntax" } )
  | Ltl.Error.SyntaxError msg ->
      let pos = buf.lex_start_p in
      raise
        (PositionedParseError
           {line= pos.pos_lnum; column= pos.pos_cnum; message= msg} )

(** The [Model] represents the full state of the application. The module has
    methods for updating the model as well, which will be used when applying
    actions, in the [apply_action] function below. *)
module Model = struct
  type t =
    { formula_error: string option
    ; pending: string
    ; formulas: Ltl.Formula.formula list
    ; trace: Ltl.Trace.trace }
  [@@deriving fields, equal]

  let cutoff t1 t2 = equal t1 t2

  let set_pending str t = {t with pending= str}

  let set_error err t = {t with formula_error= Some err}

  let clear_error t = {t with formula_error= None}

  let add_new_formula formula t =
    {t with formulas= List.append t.formulas [formula]}

  let add_state t =
    {t with trace= List.append t.trace [Ltl.Trace.State.empty]}

  let remove_formula i t =
    { t with
      formulas=
        (let before, after = List.split_n t.formulas (i + 1) in
         List.drop_last_exn before @ after ) }

  let toggle_state_atomic index atom t =
    { t with
      trace=
        List.mapi t.trace ~f:(fun i s ->
            if i = index then
              if Ltl.Trace.State.mem atom s then
                Ltl.Trace.State.remove atom s
              else Ltl.Trace.State.add atom s
            else s ) }
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
    | Toggle_state_atomic of {index: int; atom: char}
  [@@deriving sexp]
end

(** The state is for holding real imperative state, like RPC connections. We
    have none of those here, so we make it trivial. *)
module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ =
  match (action : Action.t) with
  | Action.Add_formula -> (
    try
      model
      |> Model.add_new_formula (parse (Model.pending model))
      |> Model.clear_error |> Model.set_pending ""
    with PositionedParseError {line; column; message} ->
      model
      |> Model.set_error (Printf.sprintf "%d:%d: %s" line column message) )
  | Action.Set_pending {formula} ->
      model |> Model.set_pending formula |> Model.clear_error
  | Action.Remove_formula {index} ->
      model |> Model.remove_formula index |> Model.clear_error
  (* TODO: implement *)
  | Action.Add_state -> model |> Model.add_state
  | Action.Toggle_state_atomic {index; atom} ->
      model |> Model.toggle_state_atomic index atom

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let delimiter t = Node.span ~attr:(Attr.class_ "delimiter") [Node.text t]

let literal t = Node.span ~attr:(Attr.class_ "literal") [Node.text t]

let unary_operator t =
  Node.span ~attr:(Attr.classes ["unary"; "operator"]) [Node.text t]

let binary_operator t =
  Node.span ~attr:(Attr.classes ["binary"; "operator"]) [Node.text t]

let in_parens param s =
  if param then
    Node.span ~attr:(Attr.class_ "parens") [delimiter "("; s; delimiter ")"]
  else s

let all_atoms (m : Model.t) =
  List.sort ~compare:Char.compare
    (Ltl.Formula.Names.elements
       (List.fold_left ~init:Ltl.Formula.Names.empty
          ~f:Ltl.Formula.Names.union
          (List.map ~f:Ltl.Formula.atomic_names m.formulas) ) )

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
        (Node.span
           [ unary_operator op_str
           ; Node.text " "
           ; print_formula_html f' ~param:true ] )
  | Bin_op (op, f1, f2) ->
      let op_str =
        match op with
        | And -> "&&"
        | Or -> "||"
        | Implies -> "=>"
        | Until -> "until"
      in
      in_parens param
        (Node.span
           [ print_formula_html f1 ~param:true
           ; Node.text " "
           ; binary_operator op_str
           ; Node.text " "
           ; print_formula_html f2 ~param:true ] )

let table_trace_headers (trace : Ltl.Trace.trace) =
  let last = List.length trace - 1 in
  List.mapi trace ~f:(fun i _ ->
      Node.th
        [ Node.text "S"
        ; Node.span ~attr:(Attr.class_ "subscript")
            [Node.text (Int.to_string i ^ if i = last then "..∞" else "")] ] )

let table_trace_toggles ?(disabled = false)
    ?(on_toggle = fun (_i : int) -> Effect.Ignore) ~(id_prefix : string)
    (states : bool list) =
  List.mapi states ~f:(fun i checked ->
      let id = id_prefix ^ Int.to_string i in
      Node.td ~attr:(Attr.class_ "state")
        [ Node.input
            ~attr:
              (Attr.many
                 (List.concat
                    [ [ Attr.type_ "checkbox"
                      ; Attr.id id
                      ; Attr.on_change (fun _ _ -> on_toggle i) ]
                    ; (if checked then [Attr.checked] else [])
                    ; (if disabled then [Attr.disabled] else []) ] ) )
            []
        ; Node.label ~attr:(Attr.for_ id) [] ] )

let view (m : Model.t) ~inject =
  let open Vdom in
  let add_new_formula_button =
    Node.div
      [ Node.input
          ~attr:(Attr.many [Attr.type_ "submit"; Attr.value "Add formula"])
          [] ]
  in
  let on_submit_formula =
    Attr.on_submit (fun _ev ->
        Effect.Many [Effect.Prevent_default; inject Action.Add_formula] )
  in
  let pending_formula_input =
    (* TODO: Clear input field on input "submit" *)
    Node.input
      ~attr:
        (Attr.many
           [ Attr.class_ "pending-formula"
           ; Attr.class_ "formula"
           ; Attr.value (Model.pending m)
           ; Attr.on_input (fun _ formula ->
                 inject (Action.Set_pending {formula}) ) ] )
      []
  in
  let messages =
    Node.div ~attr:(Attr.class_ "error")
      (match m.formula_error with Some msg -> [Node.text msg] | None -> [])
  in
  let add_new_form =
    Node.create "form"
      ~attr:(Attr.many [Attr.class_ "add-new"; on_submit_formula])
      [pending_formula_input; add_new_formula_button; messages]
  in
  let remove_formula_button txt ~index =
    let on_click _ev = inject (Action.Remove_formula {index}) in
    Node.button ~attr:(Attr.on_click on_click) [Node.text txt]
  in
  let atomics_and_formulas =
    let atomics_rows =
      Node.tr
        ( Node.th [Node.text "Atom"]
        :: List.append
             (table_trace_headers m.trace)
             [ Node.th ~attr:(Attr.class_ "actions")
                 [ Node.button
                     ~attr:(Attr.on_click (fun _ -> inject Action.Add_state))
                     [Node.text "+"] ] ] )
      ::
      ( match all_atoms m with
      | [] ->
          [ Node.tr
              [ Node.td
                  ~attr:(Attr.create "colspan" "2")
                  [ Node.p ~attr:(Attr.class_ "missing")
                      [Node.text "No atomics are used."] ] ] ]
      | atoms ->
          List.map atoms ~f:(fun atom ->
              Node.tr
                ( Node.td ~attr:(Attr.class_ "literal")
                    [Node.code [Node.text (String.of_char atom)]]
                :: List.append
                     (table_trace_toggles
                        ~on_toggle:(fun index ->
                          inject (Action.Toggle_state_atomic {index; atom})
                          )
                        ~id_prefix:(String.of_char atom)
                        (List.map m.trace ~f:(Ltl.Trace.State.mem atom)) )
                     [Node.td []] ) ) )
    in
    let formulas_rows =
      Node.tr
        ( Node.th [Node.text "Formula"]
        :: List.append (table_trace_headers m.trace) [Node.th []] )
      ::
      ( match m.formulas with
      | [] ->
          [ Node.tr
              [ Node.td
                  ~attr:(Attr.create "colspan" "2")
                  [ Node.p ~attr:(Attr.class_ "missing")
                      [Node.text "No formulas exist."] ] ] ]
      | _ ->
          List.mapi m.formulas ~f:(fun index formula ->
              Node.tr
                ( Node.td ~attr:(Attr.class_ "formula")
                    [Node.code [print_formula_html formula]]
                :: List.append
                     (table_trace_toggles ~disabled:true ~id_prefix:""
                        (Ltl.Eval.EvalTrace.eval_all formula m.trace) )
                     [ Node.td ~attr:(Attr.class_ "actions")
                         [remove_formula_button "Remove" ~index] ] ) ) )
    in
    Node.table (List.append atomics_rows formulas_rows)
  in
  Node.div
    ~attr:(Attr.class_ "visualizer")
    [ Node.header
        [ Node.h1 ~attr:(Attr.class_ "title")
            [Node.text "Linear Temporal Logic Visualizer (v2)"] ]
    ; Node.main [atomics_and_formulas]
    ; Node.footer [add_new_form] ]

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
  { formula_error= None
  ; pending= ""
  ; Model.formulas
  ; trace=
      [Ltl.Trace.State.empty; Ltl.Trace.State.empty; Ltl.Trace.State.empty]
  }
