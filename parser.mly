%{
  open Types
  let u = P([]);;
  let b = ref u;;
%}

%token <string>VAR
%token <string>STRING
%token RULE
%token COMMA SEMICOLON
%token PERIOD
%token LPAREN RPAREN R_SQR L_SQR
%token EOF ERROR EOL

%start toplevel 
%start interpreter
%start file
%type <Types.goal> toplevel
%type <Types.program> interpreter
%type <string> file

%right SEMICOLON
%nonassoc PERIOD GOAL IMPLIED
%right COMMA
%%
interpreter: 
  | exprtop EOF                                 { P($1) }
;
toplevel:                                     
  | temp PERIOD                                 { G($1) }
;
file:
  | L_SQR STRING PERIOD STRING R_SQR PERIOD                         { $2 ^ "." ^ $4 }
;
exprtop: clause                                 { [$1] }
  | clause exprtop                              { $1 :: $2 }
;
clause: fact                                    { F($1) }
  | rule                                        { R($1) }
;
fact: head PERIOD                               { H($1) }
;
rule: head RULE body PERIOD                     { HB($1, $3) }
;
head: atomic_F                                  { AF($1) }    
;
body: temp                                      { AFL($1) }
;
temp: atomic_F                                  { [$1] }
  | atomic_F COMMA temp                         { $1 :: $3 } 
  | atomic_F SEMICOLON temp                     { $1 :: $3 }
;
atomic_F: STRING LPAREN term_list RPAREN        { For(Sym($1), $3); }
;
term_list: term                                 { [$1] }
  | term COMMA term_list                        { $1 :: $3 }
;
term: STRING                                    { C(Cons($1)) }
  | VAR                                         { V(Var ($1)) }
  | STRING LPAREN term_list RPAREN              { Func(Sym($1), $3) }  
;

/*facts ---> F( H( AF( For( Sym("  "), [C( Cons(" ") )] ) ) ) )*/
/*married(asfd, asdfdf).*/
/*married(drapaudi, child(pandu, kunti))*/
/*facts ---> F( H( AF( For( Sym("married"), [C(Cons("asdf")); C(Cons("asdfdf"))] ) ) ) )*/
/*male(a, b, c) :- */