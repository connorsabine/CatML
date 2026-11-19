%{
open Lang
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token TIMES
%token PLUS
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token EOF

(* Additions: *)
%token AND
%token OR
%token NOT
%token FIRST
%token SECOND
%token FUN
%token FIX

%token COLON
%token PERIOD
%token COMMA
%token ARROW

%token TYPEINT
%token TYPEBOOL

(* Precedence: *)
%nonassoc IN
%nonassoc ELSE
%nonassoc ARROW
%left OR
%left AND
%left EQUALS
%left PLUS
%left TIMES

%start <Lang.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;


(* Everything Built from Apps *)
expr:
  | e1 = expr; EQUALS; e2 = expr { Equal (e1, e2) }
  | e1 = expr; TIMES;  e2 = expr { Mult(e1, e2) }
  | e1 = expr; PLUS;   e2 = expr { Plus(e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr   { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }

  | e = app {e}
  | e1 = expr; AND; e2 = expr {And(e1, e2)}
  | e1 = expr; OR; e2 = expr {Or(e1, e2)}
  | FUN; LPAREN; x = ID; COLON; t = _type; RPAREN; ARROW; e = expr {Fun(x, t, e)}
  | FIX; LPAREN; f = ID; COLON; tx = _prod_type; ARROW; tr = _type; RPAREN; PERIOD; x = ID; ARROW; e = expr {Fix(f, x, tx, tr, e)}
  ;


(* Function App, Not, First, Second *)
app:
  | e = atom {e}
  | e1 = app; e2 = atom {App(e1, e2)}
  | NOT; e = atom {Not e}
  | FIRST; e = atom {First e}
  | SECOND; e = atom {Second e}


(* Smallest Pieces *)
atom:
  | i    = INT { Int i }
  | x    = ID  { Var x }
  | TRUE       { Bool true }
  | FALSE      { Bool false }
  | LPAREN; e=expr; RPAREN {e}
  | LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN {Pair(e1, e2)}


(* Types *)
_type:
  | t = _prod_type {t}
  | t1 = _prod_type; ARROW; t2 = _type {FUN(t1, t2)}

_prod_type:
  | t = _atom_type {t}
  | t1 = _prod_type; TIMES; t2 = _atom_type {PAIR(t1, t2)}

_atom_type:
  | TYPEINT {INT}
  | TYPEBOOL {BOOL}
  | LPAREN; t = _type; RPAREN {t}