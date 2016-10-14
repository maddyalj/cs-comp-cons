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
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
    | "/*"     { comment lexbuf }
    | white    { read lexbuf }
    | newline  { next_line lexbuf; read lexbuf }
    | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "const"  { CONST }
    | "let"    { LET }
    | "print"  { PRINT }
    | "if"     { IF }
    | "else"   { ELSE }
    | "while"  { WHILE }
    | id       { ID (Lexing.lexeme lexbuf) }
    | ','      { COMMA }
    | '('      { LPAREN }
    | ')'      { RPAREN }
    | '{'      { LBRACE }
    | '}'      { RBRACE }
    | ';'      { SEMICOLON }
    | '+'      { PLUS }
    | '-'      { MINUS }
    | '*'      { TIMES }
    | '/'      { DIVIDE }
    | "<="     { LTEQ }
    | '<'      { LT }
    | ">="     { GTEQ }
    | '>'      { GT }
    | "=="     { EQ }
    | "!="     { NOTEQ }
    | "&&"     { AND }
    | "||"     { OR }
    | '!'      { NOT }
    | '='      { EQUALS }
    | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof      { EOF }
and comment = shortest
    | _* "*/"  { read lexbuf }
