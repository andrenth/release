{

open Release_config_parser

let log_level_of_string = function
  | "debug" -> Lwt_log.Debug
  | "info" -> Lwt_log.Info
  | "notice" -> Lwt_log.Notice
  | "warning" -> Lwt_log.Warning
  | "error" -> Lwt_log.Error
  | "fatal" -> Lwt_log.Fatal
  | s -> failwith ("invalid log level: " ^ s)

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
let log_level = "debug" | "info" | "notice" | "warning" | "error" | "fatal"
let ident = ['A'-'Z' 'a'-'z'][^ '#' '=' '[' ']' ' ' '\t' '\n']+

rule token = parse
  | [' ' '\t']             { token lexbuf }
  | '\n'                   { Lexing.new_line lexbuf; NEWLINE }
  | '='                    { EQUALS }
  | '['                    { LBRACKET }
  | ']'                    { RBRACKET }
  | ','                    { COMMA }
  | '#'                    { comment lexbuf }
  | digit+ as i            { INTEGER (int_of_string i) }
  | "." digit+
  | digit+ "." digit+ as f { FLOAT (float_of_string f) }
  | boolean as b           { BOOL (bool_of_string b) }
  | log_level as l         { LOG_LEVEL (log_level_of_string l) }
  | '"'                    { STRING (str (Buffer.create 16) lexbuf) }
  | '/'                    { REGEXP (regexp (Buffer.create 16) lexbuf) }
  | ident as id            { IDENT id }
  | _                      { token lexbuf }
  | eof                    { raise End_of_file }

and comment = parse
  | '\n'                   { Lexing.new_line lexbuf; NEWLINE }
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
  | _ as c
      {
        Buffer.add_char buf c;
        regexp buf lexbuf
      }
