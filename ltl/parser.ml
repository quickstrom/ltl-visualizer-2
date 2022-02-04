open Angstrom

let whitespace = skip_while (String.contains " \r\n\t")

let parens p = char '(' *> p <* char ')'

let atomic =
  let low = Char.code 'A' in
  let high = Char.code 'Z' in
  lift Formula.atomic
    (satisfy (fun c ->
         let code = Char.code c in
         code >= low && code <= high ) )

let top = (string "true" <|> string "⊤") *> return Formula.Top

let bottom = (string "false" <|> string "⊥") *> return Formula.Top

let negation p =
  (string "!" <|> string "¬") *> lift (Formula.un_op Formula.Not) p

let bin_op p1 p2 =
  let op f o = f <$> (p1 <* whitespace) <*> string o *> whitespace *> p2 in
  op (Formula.bin_op Formula.or_) "||"
  <|> op (Formula.bin_op Formula.and_) "&&"
  <|> op (Formula.bin_op Formula.implies) "=>"
  <|> op (Formula.bin_op Formula.until) "until"

let formula =
  fix (fun formula ->
      atomic <|> top <|> bottom <|> negation formula <|> parens formula
      <|> bin_op formula formula )

let parse str = Angstrom.parse_string ~consume:All formula str

exception ParseError of string

let parse_exn str = match parse str with
  | Ok(f) -> f
  | Error(e) -> raise (ParseError e)
