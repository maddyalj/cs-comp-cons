open Lexer
open Lexing
open Core.Std

let print_position lexbuf =
    let pos = lexbuf.lex_curr_p in
        eprintf "%s:%d:%d\n" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
    try Parser.prog Lexer.read lexbuf with
        | SyntaxError msg ->
            prerr_string (msg ^ ": ");
            print_position lexbuf;
            exit (-1)
        | Parser.Error ->
            prerr_string "Parse error: ";
            print_position lexbuf;
            exit (-1)

let rec parse_and_print use_eval lexbuf =
    match parse_with_error lexbuf with
        | [] -> ()
        | p  ->
            if use_eval then printf "%d\n" (Lang.eval_prog p) else printf "√ ok\n";
            parse_and_print use_eval lexbuf

let loop use_eval filename () =
    let inx = In_channel.create filename in
        let lexbuf = Lexing.from_channel inx in
            lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
            parse_and_print use_eval lexbuf;
            In_channel.close inx

let _ =
    Command.basic
        ~summary:"CS Assignment: Parser tool for our special language"
        Command.Spec.(
            empty
            +> flag "-e" no_arg ~doc:" evaluate expressions"
            +> anon ("filename" %: file)
        )
        loop
    |> Command.run
