%{

open Release_config_types
module G = Release_config_global

let current_line () =
  let start_pos = Parsing.symbol_start_pos () in
  start_pos.Lexing.pos_lnum

let section s =
  if G.is_new_section s then
    G.section s
  else
    G.duplicate_section s (current_line ())

let key k v =
  if G.is_new_key k then
    G.add k v
  else
    G.duplicate_key k (current_line ())

let () = section G.global_section

%}

%token <int> INTEGER
%token <float> FLOAT
%token <bool> BOOL
%token <string> WORD
%token EQUALS
%token NEWLINE
%token COMMENT
%token LBRACKET
%token RBRACKET

%start input
%type <unit> input

%%

input: /* empty */           { }
     | input line            { }
     | error NEWLINE         { G.syntax_error (current_line ()) }
     ;

line: NEWLINE                { }
    | COMMENT                { }
    | LBRACKET WORD RBRACKET { section $2 }
    | keyval NEWLINE         { }
    | keyval COMMENT         { }
    ;

keyval: WORD EQUALS INTEGER  { key $1 (`Int $3) }
      | WORD EQUALS FLOAT    { key $1 (`Float $3) }
      | WORD EQUALS BOOL     { key $1 (`Bool $3) }
      | WORD EQUALS WORD     { key $1 (`Str $3) }
      ;
