type op =
    | Plus
    | Minus
    | Times
    | Divide
    | Lteq
    | Lt
    | Gteq
    | Gt
    | Eq
    | Noteq
    | And
    | Or
    | Not
type exp =
    | If    of exp * exp * exp    (* if (e) { e } else { e } *)
    | While of exp * exp          (* while (e) { e } *)
    | Const of string * exp * exp (* const x = e; e *)
    | Let   of string * exp * exp (* let x = e; e *)
    | Asg   of exp * exp          (* e = e *)
    | Op    of op * exp * exp     (* e + e *)
    | Print of exp                (* print e *)
    | Appl  of string * exp list  (* foo(es) *)
    | Deref of exp                (* e *)
    | Val   of int                (* 7 *)
    | Id    of string             (* x *)
    | Seq   of exp * exp          (* e; e *)
    | Empty                       (* *)
type func = string * string list * exp
type prog = func list

let op_str = function
    | Plus   -> "Plus"
    | Minus  -> "Minus"
    | Times  -> "Times"
    | Divide -> "Divide"
    | Lteq   -> "Lteq"
    | Lt     -> "Lt"
    | Gteq   -> "Gteq"
    | Gt     -> "Gt"
    | Eq     -> "Eq"
    | Noteq  -> "Noteq"
    | And    -> "And"
    | Or     -> "Or"
    | Not    -> "Not"

open Core.Std.Printf
let rec exp_str = function
    | If (e1, e2, e3)   -> sprintf "If (%s, %s, %s)" (exp_str e1) (exp_str e2) (exp_str e3)
    | While (e1, e2)    -> sprintf "While (%s, %s)" (exp_str e1) (exp_str e2)
    | Const (s, e1, e2) -> sprintf "Const (\"%s\", %s, %s)" s (exp_str e1) (exp_str e2)
    | Let (s, e1, e2)   -> sprintf "Let (\"%s\", %s, %s)" s (exp_str e1) (exp_str e2)
    | Asg (e1, e2)      -> sprintf "Asg (%s, %s)" (exp_str e1) (exp_str e2)
    | Op (op, e1, e2)   -> sprintf "Op (%s, %s, %s)" (op_str op) (exp_str e1) (exp_str e2)
    | Print e           -> sprintf "Print (%s)" (exp_str e)
    | Appl (s, es)      -> sprintf "Appl (\"%s\", %s)" s ("[" ^ (String.concat ", " (List.map exp_str es)) ^ "]")
    | Deref e           -> sprintf "Deref (%s)" (exp_str e)
    | Val n             -> sprintf "Val %d" n
    | Id x              -> sprintf "Id \"%s\"" x
    | Seq (e1, e2)      -> sprintf "Seq (%s, %s)" (exp_str e1) (exp_str e2)
    | Empty             -> sprintf "Empty"

let prog_str p =
    let func_str (f, ps, e) = f ^ "(" ^ (String.concat ", " ps) ^ "):\n    " ^ (exp_str e) ^ "\n" in
        String.concat "" (List.map func_str p)

let rec get_func x = function
    | []              -> raise Not_found
    | (f, p, e) :: fs -> if (String.compare f x) == 0 then (f, p, e) else get_func x fs

let eval_op x y =
    let bool_int b = if b then 1 else 0
    and int_bool i = if i > 0 then true else false in function
        | Plus   -> x + y
        | Minus  -> x - y
        | Times  -> x * y
        | Divide -> x / y
        | Lteq   -> (x <= y) |> bool_int
        | Lt     -> (x <  y) |> bool_int
        | Gteq   -> (x >= y) |> bool_int
        | Gt     -> (x >  y) |> bool_int
        | Eq     -> (x == y) |> bool_int
        | Noteq  -> (x != y) |> bool_int
        | And    -> (int_bool x && int_bool y) |> bool_int
        | Or     -> (int_bool x || int_bool y) |> bool_int
        | Not    -> (not (int_bool x)) |> bool_int

let oi = ref 0
let rec eval_exp o prog store =
    let exp_bool e = if (eval_exp o prog store e) > 0 then true else false in function
        | If (e1, e2, e3)   -> if exp_bool e1 then eval_exp o prog store e2 else eval_exp o prog store e3
        | While (e1, e2)    -> while exp_bool e1 do (let _ = eval_exp o prog store e2 in ()) done; 0
        | Const (s, e1, e2) -> let v1 = eval_exp o prog store e1 in Hashtbl.replace store s v1; eval_exp o prog store e2
        | Let (s, e1, e2)   -> let v1 = eval_exp o prog store e1 in Hashtbl.replace store s v1; eval_exp o prog store e2
        | Asg (Id x, e2)    -> let v2 = eval_exp o prog store e2 in Hashtbl.replace store x v2; 0
        | Op (op, e1, e2)   -> eval_op (eval_exp o prog store e1) (eval_exp o prog store e2) op
        | Print e           -> printf "%s\n" (string_of_int (eval_exp o prog store e)); 0
        | Appl (s, es)      -> (match s with
            | "read_int" -> 0
            | "dump" -> List.iter (fun e -> printf "%s\n" (string_of_int (eval_exp o prog store e))) es; 0
            | _ -> let (_, p, e) = get_func s prog and st = Hashtbl.create 100 in
                let st_p i par = Hashtbl.replace st par (eval_exp o prog store (List.nth es i)) in
                    List.iteri st_p p;
                    eval_exp o prog st e
        )
        | Deref (Id x)      -> Hashtbl.find store x
        | Val n             -> n
        | Seq (e1, e2)      -> let _ = eval_exp o prog store e1 in let v2 = eval_exp o prog store e2 in v2
        | _                 -> 0

let rec rep_with_val id n = function
    | If (e1, e2, e3)   -> If (rep_with_val id n e1, rep_with_val id n e2, rep_with_val id n e3)
    | While (e1, e2)    -> While (rep_with_val id n e1, rep_with_val id n e2)
    | Const (s, e1, e2) -> Const (s, rep_with_val id n e1, rep_with_val id n e2)
    | Let (s, e1, e2)   -> Let (s, rep_with_val id n e1, rep_with_val id n e2)
    | Asg (Id x, e2)    -> Asg (Id x, rep_with_val id n e2)
    | Op (op,  e1, e2)  -> Op (op, rep_with_val id n e1, rep_with_val id n e2)
    | Print e           -> Print (rep_with_val id n e)
    | Appl (s, es)      -> Appl (s, List.map (rep_with_val id n) es)
    | Deref (Id x)      -> if (String.compare id x) == 0 then Val n else Deref (Id x)
    | Seq (e1, e2)      -> Seq (rep_with_val id n e1, rep_with_val id n e2)
    | e -> e

let rec opt_exp p = function
    | If (e1, e2, e3)        -> let e1 = opt_exp p e1 in (match e1 with
        | Val n -> oi := !oi + 1; if n > 0 then opt_exp p e2 else opt_exp p e3
        | _     -> If (e1, opt_exp p e2, opt_exp p e3)
    )
    | While (e1, e2)         -> While (opt_exp p e1, opt_exp p e2)
    | Const (s, e1, e2)      -> let e1 = opt_exp p e1 in (match e1 with
        | Val n -> oi := !oi + 1; opt_exp p (rep_with_val s n e2)
        | _     -> Const (s, e1, opt_exp p e2)
    )
    | Let (s, e1, e2)        -> Let (s, opt_exp p e1, opt_exp p e2)
    | Asg (Id x, e2)         -> Asg (Id x, opt_exp p e2)
    | Op (op,  Val x, Val y) -> oi := !oi + 1; Val (eval_op x y op)
    | Op (Not, Val x, _)     -> oi := !oi + 1; Val (eval_op x 0 Not)
    | Op (op,  e1, e2)       -> Op (op, opt_exp p e1, opt_exp p e2)
    | Print e                -> Print (opt_exp p e)
    | Appl (s, es)           -> let es = List.map (opt_exp p) es in
        if List.for_all (fun e -> match e with | Val _ -> true | _ -> false) es
        then (
            try let (_, pars, e) = get_func s p in let e = ref e and get_val = function | Val n -> n | _ -> raise Not_found in
                oi := !oi + 1;
                List.iteri (fun i par -> e := rep_with_val par (get_val (List.nth es i)) !e) pars;
                opt_exp p !e
            with Not_found -> Appl (s, es)
        )
        else Appl (s, es)
    | Seq (e1, e2)           -> let e1 = opt_exp p e1 in (match e1 with
        | Val _ -> oi := !oi + 1; opt_exp p e2
        | _     -> Seq (e1, opt_exp p e2)
    )
    | e -> e

let eval_prog prog o =
    let (_, _, e) = get_func "main" prog in
        let exp = if o then opt_exp prog e else e in
            let result = eval_exp o prog (Hashtbl.create 100) exp in
                if o then (printf "(#opt %d) " !oi; result) else result
