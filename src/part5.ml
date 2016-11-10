open Lang
open Printf

let i = ref 0
let e = ref 0

let interpret_test p expected =
    addr_base := 0;
    let result = Hashtbl.find ram (interpret (Hashtbl.create 100) [] p) in
        printf "Interpret Result = %d\n" result;
        printf "Expected Result  = %d\n" expected;
        if result == expected then
            printf "√ Correct\n\n"
        else
            (e := !e + 1;
            printf "× FAILED\n\n")

let codegen_test p =
    Buffer.reset code;
    addr_base := 0;
    let addr = codegen (Hashtbl.create 100) [] p in
        Buffer.output_buffer stdout code;
        printf "ld  r%i\n" (addr)

let _ =
    List.iter (fun (ex, p) ->
        i := !i + 1;
        printf "TEST #%d\n" !i;
        codegen_test p;
        interpret_test p ex
    ) [
        (* Test 1 *)
        (140, Const (
            "X",
            Op (Plus, Val 30, Val 40),
            Op (Plus, Id "X", Id "X")
        ));

        (* Test 2 *)
        (5, Const (
            "A",
            Val 10,
            Seq (
                Op (Plus, Id "A", Val 5),
                Op (Divide, Id "A", Val 2)
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
            Const (
                "PRO",
                Val 5,
                Op (Times, Id "ENV", Id "PRO")
            )
        ));

        (* Test 5 *)
        (-9, Let (
            "x",
            Val 50,
            Seq (
                Asg (
                    Id "x",
                    Op (Minus, Val 5, Id "x")
                ),
                Op (Divide, Id "x", Val 5)
            )
        ));
    ];
    if !e == 0 then
        printf "All tests passed!\n"
    else
        printf "%i TESTS FAILED!!\n" !e
