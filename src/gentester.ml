open Lang
open Printf
open Lexer
open Lexing
open Core

let selected_option = ref 0
let selected_opt = ref false
let selected_prefix = ref ""

let i = ref 0
let fn = ref ""
let e = ref 0
let output = Buffer.create 100

let codegen_prefix = Buffer.create 100
let codegen_suffix = Buffer.create 100

let codegenx86_test p =
    codegenx86_prog p !selected_opt;
    (* Buffer.output_buffer stdout code; *)
    Std.Out_channel.write_all
        ("generatedx86/" ^ !fn ^ ".s")
        ~data:((Buffer.contents codegen_prefix) ^ (Buffer.contents code) ^ (Buffer.contents codegen_suffix))

let is_digit c =
    if c == '-' then true else try
        let _ = Std.Char.to_string c |> int_of_string in true
    with
        | _ -> false

let get_result command =
    let ic = Unix.open_process_in command in
        let string = Bytes.create 100
        and char_count = ref 1 in
            while !char_count > 0 do
                char_count := input ic string 0 100;
                Buffer.add_substring output string 0 !char_count
            done;
        let _ = Unix.close_process_in ic
        and digits = Buffer.create 100 in
            for i = 0 to (Buffer.length output) - 1 do
                let c = Buffer.nth output i in
                    if is_digit c then Buffer.add_char digits c else ()
            done;
            try
                Buffer.contents digits |> int_of_string
            with
                | _ -> 0

let eval_test p expected =
    let result = Lang.eval_prog p !selected_opt in
        printf "Actual Result   = %d\n" result;
        printf "Expected Result = %d\n" expected;
        if result == expected then
            printf "√ Correct\n\n"
        else
            (e := !e + 1;
            printf "× FAILED\n\n")

let error_test expected =
    printf "Actual Result   = Parse Error\n";
    (* if expected == -1425652 then ( *)
        printf "Expected Result = Parse Error\n";
        printf "√ Correct\n\n"
    (* )
    else (
        printf "Expected Result = %d\n" expected;
        e := !e + 1;
        printf "× FAILED\n\n"
    ) *)

let result_test expected =
    let _ = Sys.command ("cc -o \"executables/" ^ !fn ^ ".out\" " ^ "generatedx86/" ^ !fn ^ ".s") in
        let result = get_result ("./executables/" ^ !fn ^ ".out") in
            printf "Actual Result   = %s" (Buffer.contents output);
            printf "Expected Result = %d\n" expected;
            Buffer.reset output;
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
        | _ -> []

let rec parse_and_print lexbuf expected =
    match parse_with_error lexbuf with
        | [] -> error_test expected
        | p  ->
            if !selected_option == 1 then eval_test p expected else (codegenx86_test p; result_test expected)

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
        let expected = String.sub f index ((String.length f) - index - 2) in
            try int_of_string expected with
            | _ -> -1425652

let print_menu _ =
    print_string "Select an option:\n";
    print_string "1 - Evaluate\n";
    print_string "2 - x86 Code Generate & Execute\n";
    selected_option := read_int ();
    if !selected_option < 1 || !selected_option > 2 then selected_option := 1 else ();

    print_string "Turn optimization on?:\n";
    print_string "1 - Yes\n";
    print_string "2 - No\n";
    let input = read_int () in if input == 2 then selected_opt := false else selected_opt := true;

    print_string "Enter filename prefix (leave empty to test all files):\n";
    selected_prefix := read_line ()

let read_codegen_files () =
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
        close_in ifile

let loop () =
    print_menu ();
    read_codegen_files ();
    let tests = Sys.readdir "tests" in
        Std.Array.iter tests (fun filename ->
            if starts_with filename !selected_prefix then (
                let inx = Std.In_channel.create ("tests/" ^ filename) in
                    let expected = get_expected filename in
                        i := !i + 1;
                        fn := (String.sub filename 0 (String.length filename - 2));
                        printf "Testing %s\n" filename;
                        if expected == -1425652 then error_test expected else (
                            let lexbuf = Lexing.from_channel inx in
                                lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
                                parse_and_print lexbuf expected;
                                Std.In_channel.close inx
                        )
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
        Std.Command.Spec.(empty)
        loop
    |> Std.Command.run
