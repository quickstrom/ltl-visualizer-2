open! Core
open! Incr_dom
open! Js_of_ocaml
open! Ltl.Formula.Syntax

let () =
  Start_app.start
    (module App)
    ~bind_to_element_with_id:"app"
    ~initial_model:
      (App.initial_model_exn
         [eventually (Atomic 'A' || next (Atomic 'B' && always (Atomic 'C')))] )
