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

let rec eval_exp prog store =
    let bool_int b = if b then 1 else 0
    and exp_bool e = if (eval_exp prog store e) > 0 then true else false in function
        | If (e1, e2, e3)     -> if exp_bool e1 then eval_exp prog store e2 else eval_exp prog store e3
        | While (e1, e2)      -> while exp_bool e1 do (let _ = eval_exp prog store e2 in ()) done; 0
        | Const (s, e1, e2)   -> let v1 = eval_exp prog store e1 in Hashtbl.replace store s v1; eval_exp prog store e2
        | Let (s, e1, e2)     -> let v1 = eval_exp prog store e1 in Hashtbl.replace store s v1; eval_exp prog store e2
        | Asg (Id x, e2)      -> let v2 = eval_exp prog store e2 in Hashtbl.replace store x v2; 0
        | Op (Plus,   e1, e2) -> eval_exp prog store e1 + eval_exp prog store e2
        | Op (Minus,  e1, e2) -> eval_exp prog store e1 - eval_exp prog store e2
        | Op (Times,  e1, e2) -> eval_exp prog store e1 * eval_exp prog store e2
        | Op (Divide, e1, e2) -> eval_exp prog store e1 / eval_exp prog store e2
        | Op (Lteq,   e1, e2) -> (eval_exp prog store e1 <= eval_exp prog store e2) |> bool_int
        | Op (Lt,     e1, e2) -> (eval_exp prog store e1 <  eval_exp prog store e2) |> bool_int
        | Op (Gteq,   e1, e2) -> (eval_exp prog store e1 >= eval_exp prog store e2) |> bool_int
        | Op (Gt,     e1, e2) -> (eval_exp prog store e1 >  eval_exp prog store e2) |> bool_int
        | Op (Eq,     e1, e2) -> (eval_exp prog store e1 == eval_exp prog store e2) |> bool_int
        | Op (Noteq,  e1, e2) -> (eval_exp prog store e1 != eval_exp prog store e2) |> bool_int
        | Op (And,    e1, e2) -> (exp_bool e1 && exp_bool e2) |> bool_int
        | Op (Or,     e1, e2) -> (exp_bool e1 || exp_bool e2) |> bool_int
        | Op (Not,    e,  _ ) -> exp_bool e |> not            |> bool_int
        | Print e             -> printf "%s\n" (string_of_int (eval_exp prog store e)); 0
        | Appl (s, es)        -> (match s with
            | "read_int" -> 0
            | "dump" -> List.iter (fun e -> printf "%s\n" (string_of_int (eval_exp prog store e))) es; 0
            | _ -> let (_, p, e) = get_func s prog and st = Hashtbl.create 100 in
                let st_p i par = Hashtbl.replace st par (eval_exp prog store (List.nth es i)) in
                    List.iteri st_p p;
                    eval_exp prog st e
        )
        | Deref (Id x)        -> Hashtbl.find store x
        | Val n               -> n
        | Seq (e1, e2)        -> let _ = eval_exp prog store e1 in let v2 = eval_exp prog store e2 in v2
        | _                   -> 0

let eval_prog prog = let (_, _, e) = get_func "main" prog in eval_exp prog (Hashtbl.create 100) e
