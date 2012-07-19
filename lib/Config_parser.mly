%{

open Release_config_types

let current_line () =
  let start_pos = Parsing.symbol_start_pos () in
  start_pos.Lexing.pos_lnum

let section s =
  if Config.is_new_section s then
    Config.section s
  else
    Config.duplicate_section s (current_line ())

let key k v =
  if Config.is_new_key k then
    Config.add k v
  else
    Config.duplicate_key k (current_line ())

let () = section Config.global_section

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
     | error NEWLINE         { Config.syntax_error (current_line ()) }
     ;

line: NEWLINE                { }
    | COMMENT                { }
    | LBRACKET WORD RBRACKET { section $2 }
    | keyval NEWLINE         { }
    | keyval COMMENT         { }
    ;

keyval: WORD EQUALS INTEGER  { key $1 (Int $3) }
      | WORD EQUALS FLOAT    { key $1 (Float $3) }
      | WORD EQUALS BOOL     { key $1 (Bool $3) }
      | WORD EQUALS WORD     { key $1 (String $3) }
      ;
