open QCheck
open Ltl

let gen_un_op =
  Gen.(
    oneofl [Formula.Not; Formula.Next; Formula.Always; Formula.Eventually])

let gen_bin_op =
  Gen.(oneofl [Formula.And; Formula.Or; Formula.Implies; Formula.Until])

let gen_formula =
  Gen.(
    sized
    @@ fix (fun self n ->
           match n with
           | 0 ->
               oneof
                 [ pure Formula.top
                 ; pure Formula.bottom
                 ; Formula.atomic <$> Gen.char_range 'A' 'Z' ]
           | _ ->
               oneof
                 [ pure Formula.top
                 ; pure Formula.bottom
                 ; Formula.atomic <$> Gen.char_range 'A' 'Z'
                 ; Formula.un_op <$> gen_un_op <*> self (n / 2)
                 ; Formula.bin_op <$> gen_bin_op
                   <*> self (n / 2)
                   <*> self (n / 2) ] ))

let parse str = Ltl.Parser.f Ltl.Lexer.f (Lexing.from_string str)

let test_rountrip =
  QCheck.Test.make ~count:100 ~name:"parser and printer roundtrip"
    (QCheck.make ~print:Formula.show_formula gen_formula) (fun f ->
      parse (Printer.print_formula f) = f )

let equiv f1 f2 t = Ltl.Eval.EvalTrace.eval_all f1 t =
    Ltl.Eval.EvalTrace.eval_all f2 t

let test_eval_e_u_equiv =
  QCheck.Test.make ~count:100 ~name:"eventually/until equivalence"
    (QCheck.make ~print:Formula.show_formula gen_formula) (fun f ->
      Formula.Syntax.(equiv (eventually f) (until Ltl.Formula.Top f) []))

let () = QCheck_runner.run_tests_main [test_rountrip; test_eval_e_u_equiv]
