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

open Core.Std
let rec exp_str = function
    | If (e1, e2, e3)   -> sprintf "If (%s, %s, %s)" (exp_str e1) (exp_str e2) (exp_str e3)
    | While (e1, e2)    -> sprintf "While (%s, %s)" (exp_str e1) (exp_str e2)
    | Const (s, e1, e2) -> sprintf "Const (\"%s\", %s, %s)" s (exp_str e1) (exp_str e2)
    | Let (s, e1, e2)   -> sprintf "Let (\"%s\", %s, %s)" s (exp_str e1) (exp_str e2)
    | Asg (e1, e2)      -> sprintf "Asg (%s, %s)" (exp_str e1) (exp_str e2)
    | Op (op, e1, e2)   -> sprintf "Op (%s, %s, %s)" (op_str op) (exp_str e1) (exp_str e2)
    | Print e           -> sprintf "Print (%s)" (exp_str e)
    | Appl (s, es)      -> sprintf "Appl (\"%s\", %s)" s ("[" ^ (String.concat ~sep:", " (List.map ~f:exp_str es)) ^ "]")
    | Deref e           -> sprintf "Deref (%s)" (exp_str e)
    | Val n             -> sprintf "Val %d" n
    | Id x              -> sprintf "Id \"%s\"" x
    | Seq (e1, e2)      -> sprintf "Seq (%s, %s)" (exp_str e1) (exp_str e2)
    | Empty             -> sprintf "Empty"

let prog_str p =
    let func_str (f, ps, e) = f ^ "(" ^ (String.concat ~sep:", " ps) ^ "):\n    " ^ (exp_str e) ^ "\n" in
        String.concat (List.map ~f:func_str p)
