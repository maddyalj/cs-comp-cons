open Lang
open Printf
open Lexer
open Lexing
open Core

let i = ref 0
let e = ref 0

let codegen_prefix = Buffer.create 100
let codegen_suffix = Buffer.create 100

let codegen_test p =
    codegenx86_prog p;
    Buffer.output_buffer stdout code;
    Std.Out_channel.write_all
        ("generatedx86/test_" ^ (string_of_int !i) ^ ".s")
        ~data:((Buffer.contents codegen_prefix) ^ (Buffer.contents code) ^ (Buffer.contents codegen_suffix))

let is_digit c =
    if c == '-' then true else try
        let _ = Std.Char.to_string c |> int_of_string in true
    with
        | _ -> false

let get_result command =
    let buffer = Buffer.create 100
    and ic = Unix.open_process_in command in
        let string = Bytes.create 100
        and char_count = ref 1 in
            while !char_count > 0 do
                char_count := input ic string 0 100;
                Buffer.add_substring buffer string 0 !char_count
            done;
        let _ = Unix.close_process_in ic
        and digits = Buffer.create 100 in
            for i = 0 to (Buffer.length buffer) - 1 do
                let c = Buffer.nth buffer i in
                    if is_digit c then Buffer.add_char digits c else ()
            done;
            try
                Buffer.contents digits |> int_of_string
            with
                | _ -> 0

let result_test expected =
    let _ = Sys.command ("cc -o \"executables/test_" ^ (string_of_int !i) ^ ".out\" " ^ "generatedx86/test_" ^ (string_of_int !i) ^ ".s") in
    let result = get_result ("./executables/test_" ^ (string_of_int !i) ^ ".out") in
        printf "Interpret Result = %d\n" result;
        printf "Expected Result  = %d\n" expected;
        if result == expected then
            printf "√ Correct\n\n"
        else
            (e := !e + 1;
            printf "× FAILED\n\n")

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

let rec parse_and_print lexbuf expected =
    match parse_with_error lexbuf with
        | [] -> ()
        | p  ->
            codegen_test p;
            result_test expected;
            parse_and_print lexbuf expected

let rec starts_with s prefix =
    if (String.length prefix) > 0 then (
        try
            if (prefix.[0] == s.[0]) then
                starts_with (String.sub s 1 (String.length s - 1)) (String.sub prefix 1 (String.length prefix - 1))
            else false
        with
            | _ -> false
    ) else true

let get_expected f =
    let index = (String.rindex f '_') + 1 in
        String.sub f index ((String.length f) - index - 2) |> int_of_string

let loop prefix () =
    let ifile = open_in "src/codegen_prefix" in
    try
        while true; do
            Buffer.add_string codegen_prefix ((input_line ifile) ^ "\n")
        done
    with End_of_file ->
        close_in ifile;
    ;
    let ifile = open_in "src/codegen_suffix" in
    try
        while true; do
            Buffer.add_string codegen_suffix ((input_line ifile) ^ "\n")
        done
    with End_of_file ->
        close_in ifile;
    ;
    let tests = Sys.readdir "tests" in
        Std.Array.iter tests (fun filename ->
            if starts_with filename prefix then (
                let inx = Std.In_channel.create ("tests/" ^ filename) in
                    let lexbuf = Lexing.from_channel inx in
                        i := !i + 1;
                        printf "Testing %s\n" filename;
                        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
                        parse_and_print lexbuf (get_expected filename);
                        Std.In_channel.close inx
            ) else ()
        )
    ;
    if !e == 0 then
        printf "All tests passed!\n"
    else
        printf "%i TESTS FAILED!!\n" !e

let _ =
    Std.Command.basic
        ~summary:"x86 Code Generator tester for our special language"
        Std.Command.Spec.(
            empty
            +> anon ("prefix" %: file)
        )
        loop
    |> Std.Command.run
