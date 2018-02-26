{

open Config_parser

let char_for_backslash = function
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 'b' -> '\b'
  | 't' -> '\t'
  | c   -> c

let char_for_decimal lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48)
        +  10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48)
        +       (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  Char.chr c

let char_for_hexadecimal lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 =
    if d1 >= 97 then d1 - 87
    else if d1 >= 65 then d1 - 55
    else d1 - 48 in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 =
    if d2 >= 97 then d2 - 87
    else if d2 >= 65 then d2 - 55
    else d2 - 48 in
  Char.chr (val1 * 16 + val2)

}

let digit = ['0'-'9']
let boolean = "true" | "false"
let ident = ['A'-'Z' 'a'-'z'][^ '#' '=' '[' ']' ' ' '\t' '\n']+

rule token = parse
  | [' ' '\t']             { token lexbuf }
  | '\n'                   { Lexing.new_line lexbuf; token lexbuf }
  | '='                    { EQUALS }
  | '['                    { LBRACKET }
  | ']'                    { RBRACKET }
  | ','                    { COMMA }
  | '#'                    { comment lexbuf }
  | digit+ as i            { INTEGER (int_of_string i) }
  | "." digit+
  | digit+ "." digit+ as f { FLOAT (float_of_string f) }
  | boolean as b           { BOOL (bool_of_string b) }
  | '"'                    { STRING (str (Buffer.create 16) lexbuf) }
  | '/'                    { REGEXP (regexp (Buffer.create 16) lexbuf) }
  | ident as id            { IDENT id }
  | eof                    { raise End_of_file }

and comment = parse
  | '\n'                   { Lexing.new_line lexbuf; token lexbuf }
  | _                      { comment lexbuf }

and str buf = parse
  | '"'
      {
        Buffer.contents buf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      {
        Buffer.add_char buf (char_for_backslash (Lexing.lexeme_char lexbuf 1));
        str buf lexbuf
      }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      {
        Buffer.add_char buf (char_for_decimal lexbuf 1);
        str buf lexbuf
      }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      {
        Buffer.add_char buf (char_for_hexadecimal lexbuf 1);
        str buf lexbuf
      }
  | _ as c
      {
        Buffer.add_char buf c;
        str buf lexbuf
      }

and regexp buf = parse
  | '/'
      {
        Str.regexp (Buffer.contents buf)
      }
  | '(' as p
  | ')' as p
  | '|' as p
      {
        Buffer.add_string buf "\\";
        Buffer.add_char buf p;
        regexp buf lexbuf
      }
  | '\\' '/'
      {
        Buffer.add_char buf '/';
        regexp buf lexbuf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      {
        Buffer.add_char buf (char_for_backslash (Lexing.lexeme_char lexbuf 1));
        regexp buf lexbuf
      }
  | '\n'
      {
        strip buf lexbuf
      }
  | _ as c
      {
        Buffer.add_char buf c;
        regexp buf lexbuf
      }

and strip buf = parse
  | [' ' '\t' '\n']
      {
        strip buf lexbuf
      }
  | _
      {
        let module L = Lexing in
        lexbuf.L.lex_curr_pos <- lexbuf.L.lex_curr_pos - 1;
        let p = lexbuf.L.lex_curr_p in
        lexbuf.L.lex_curr_p <- { p with L.pos_cnum = p.L.pos_cnum - 1 };
        regexp buf lexbuf
      }
