{

open Release_config_parser

let incr_line_number lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1
  ; Lexing.pos_bol  = pos.Lexing.pos_cnum
  }

}

let digit = ['0'-'9']
let boolean = "true" | "false"
let word = [^ '=' '[' ']' ' ' '\t' '\n']+
let comment = '#'[^ '\n']*

rule token = parse
  | [' ' '\t']             { token lexbuf }
  | '\n'                   { incr_line_number lexbuf; NEWLINE }
  | '='                    { EQUALS }
  | '['                    { LBRACKET }
  | ']'                    { RBRACKET }
  | comment                { COMMENT }
  | digit+ as i            { INTEGER (int_of_string i) }
  | "." digit+
  | digit+ "." digit+ as f { FLOAT (float_of_string f) }
  | boolean as b           { BOOL (bool_of_string b) }               
  | word as w              { WORD w }
  | _                      { token lexbuf }
  | eof                    { raise End_of_file }
