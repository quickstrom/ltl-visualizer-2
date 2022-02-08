%{
exception ParseError of string
%}

%token <char> ATOMIC
%token <string> IDENT
%token LPAREN
%token RPAREN
%token TOP
%token BOTTOM
%token EOF

%start <Formula.formula> f

%%

f : e = expr; EOF { e }

expr :
  | LPAREN; e = expr; RPAREN; { e }
  | a = ATOMIC { Formula.Atomic a }
  | TOP { Formula.Top }
  | BOTTOM { Formula.Bottom }
  | i = IDENT; e = expr; {
        let op = match i with
          | "not" -> Formula.Not
          | "next" -> Formula.Next
          | "always" -> Formula.Always
          | "eventually" -> Formula.Eventually
          | _ -> raise (ParseError ("Invalid unary operator: " ^ i))
        in Formula.Un_op (op, e)
      }
  | e1 = expr; i = IDENT; e2 = expr; {
        let op = match i with
          | "&&" -> Formula.And
          | "||" -> Formula.Or
          | "->" -> Formula.Implies
          | "until" -> Formula.Until
          | _ -> raise (ParseError ("Invalid binary operator: " ^ i))
        in Formula.Bin_op (op, e1, e2)
      }
