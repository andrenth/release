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
%token COMMA

%start input
%type <unit> input

%%

input: /* empty */                { }
     | input line                 { }
     | error NEWLINE              { G.syntax_error (current_line ()) }
     ;

line: NEWLINE                     { }
    | COMMENT                     { }
    | LBRACKET WORD RBRACKET      { section $2 }
    | keyval NEWLINE              { }
    | keyval COMMENT              { }
    ;

keyval: WORD EQUALS value         { key $1 $3 }
      | WORD EQUALS listval       { key $1 $3 }
      ;

value: INTEGER                    { `Int $1 }
     | FLOAT                      { `Float $1 }
     | BOOL                       { `Bool $1 }
     | WORD                       { `Str $1 }
     ;

listval: LBRACKET values RBRACKET { `List (List.rev $2) }
       ;

values: values COMMA value { $3::$1 }
      | value              { [$1] }
      ;
