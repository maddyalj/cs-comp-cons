{
    open Lexing
    open Parser

    exception SyntaxError of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in lexbuf.lex_curr_p <-
            {
                pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
            }
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let space = white | newline
let func_sep = space* '}' space* "func"
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
    | white      { read lexbuf }
    | newline    { next_line lexbuf; read lexbuf }
    | func_sep   { FUNC_SEP }
    | int        { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "true"     { TRUE }
    | "false"    { FALSE }
    | "null"     { NULL }
    | "let"      { LET }
    | "return"   { RETURN }
    | "func"     { FUNC }
    | "if"       { IF }
    | "else"     { ELSE }
    | "while"    { WHILE }
    | identifier { ID (Lexing.lexeme lexbuf) }
    | ','        { COMMA }
    | '('        { LEFT_PAREN }
    | ')'        { RIGHT_PAREN }
    | '{'        { LEFT_BRACE }
    | '}'        { RIGHT_BRACE }
    | ';'        { SEMICOLON }
    | '+'        { PLUS }
    | '-'        { MINUS }
    | '*'        { TIMES }
    | '/'        { DIVIDE }
    | "<="       { LEQ }
    | ">="       { GEQ }
    | "=="       { EQUAL }
    | "!="       { NOTEQ }
    | "&&"       { AND }
    | "||"       { OR }
    | '!'        { NOT }
    | '='        { EQUALS }
    | _          { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof        { EOF }
