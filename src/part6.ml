open Lang
open Printf

let i = ref 0
let e = ref 0

let codegen_prefix = Buffer.create 100
let codegen_suffix = Buffer.create 100

let codegen_test p =
    Buffer.reset code;
    sp := 0;
    codegenx86 (Hashtbl.create 100) [] p;
    Core.Std.Out_channel.write_all
        ("generatedx86/test_" ^ (string_of_int !i) ^ ".s")
        ~data:((Buffer.contents codegen_prefix) ^ (Buffer.contents code) ^ (Buffer.contents codegen_suffix))

let is_digit c =
    if c == '-' then true else try
        let _ = Core.Std.Char.to_string c |> int_of_string in true
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

let _ =
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
    List.iter (fun (ex, p) ->
        i := !i + 1;
        printf "TEST #%d\n" !i;
        codegen_test p;
        result_test ex
    ) [
        (* Test 1 *)
        (140, Const (
            "X",
            Op (Plus, Val 30, Val 40),
            Op (Plus, Id "X", Id "X")
        ));

        (* Test 2 *)
        (20, Const (
            "A",
            Val 10,
            Seq (
                Op (Plus, Id "A", Val 5),
                Op (Times, Id "A", Val 2)
            )
        ));

        (* Test 3 *)
        (9, Let (
            "x",
            Op (Plus, Val 1, Val 2),
            Const (
                "Y",
                Op (Minus, Val 6, Id "x"),
                Op (Times, Id "Y", Id "x")
            )
        ));

        (* Test 4 *)
        (150, Const (
            "ENV",
            Op (Times, Val 10, Val 3),
            Op (Times, Id "ENV", Val 5)
        ));

        (* Test 5 *)
        (-14, Let (
            "x",
            Val (-10),
            Op (Minus, Id "x", Val 4)
        ));

        (* Test 6 *)
        (21, Const (
            "x",
            Val 10,
            Const (
                "y",
                Val 11,
                Op (Plus, Id "x", Id "y")
            )
        ));
    ];
    if !e == 0 then
        printf "All tests passed!\n"
    else
        printf "%i TESTS FAILED!!\n" !e
