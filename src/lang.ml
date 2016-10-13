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
    | IfElse of exp * exp list * exp list (* if (e) {es} else {es} *)
    | If     of exp * exp list            (* if (e) {es} *)
    | While  of exp * exp list            (* if (e) {es} *)
    | Const  of string * exp              (* const x = e *)
    | Let    of string * exp              (* let x = e *)
    | Asg    of exp * exp                 (* e = e *)
    | Op     of op * exp * exp            (* e + e *)
    | Print  of exp                       (* print e *)
    | Return of exp                       (* return e *)
    | Appl   of string * exp list         (* foo(es) *)
    | Deref  of exp                       (* !e *)
    | Val    of int                       (* 7 *)
    | Id     of string                    (* x *)
type func = string * string list * exp list
type prog = func list

open Core.Std
let op_str = function
    | Plus   -> "+"
    | Minus  -> "-"
    | Times  -> "*"
    | Divide -> "/"
    | Lteq   -> "<="
    | Lt     -> "<"
    | Gteq   -> ">="
    | Gt     -> "<"
    | Eq     -> "=="
    | Noteq  -> "!="
    | And    -> "&&"
    | Or     -> "||"
    | Not    -> "!"
let blk_str s f exps = String.concat ~sep:s (List.map exps ~f:f)
let rec exp_str n =
    let ind = String.make (n * 4) ' '
    and endsc = (if n == 0 then "" else ";")
    and inr_str e = blk_str "\n" (exp_str (n + 1)) e
    in function
        | IfElse (e, es1, es2) -> sprintf "%sif (%s) {\n%s\n%s} else {\n%s\n%s}" ind (exp_str 0 e) (inr_str es1) ind (inr_str es2) ind
        | If (e, es)           -> sprintf "%sif (%s) {\n%s\n%s}" ind (exp_str 0 e) (inr_str es) ind
        | While (e, es)        -> sprintf "%swhile (%s) {\n%s\n%s}" ind (exp_str 0 e) (inr_str es) ind
        | Const (s, e)         -> sprintf "%sconst %s = %s;" ind s (exp_str 0 e)
        | Let (s, e)           -> sprintf "%slet %s = %s;" ind s (exp_str 0 e)
        | Asg (e1, e2)         -> sprintf "%s%s = %s;" ind (exp_str 0 e1) (exp_str 0 e2)
        | Op (op, e1, e2)      -> sprintf "%s%s %s %s" ind (exp_str 0 e1) (op_str op) (exp_str 0 e2)
        | Print e              -> sprintf "%sprint %s;" ind (exp_str 0 e)
        | Return e             -> sprintf "%sreturn %s;" ind (exp_str 0 e)
        | Appl (s, es)         -> sprintf "%s%s(%s)%s" ind s (blk_str ", " (exp_str 0) es) endsc
        | Deref e              -> sprintf "%s%s%s" ind (exp_str 0 e) endsc
        | Val i                -> sprintf "%s%d%s" ind i endsc
        | Id s                 -> sprintf "%s%s" ind s
let func_str (id, params, exps) =
    let ps = String.concat ~sep:", " params in
        sprintf "%s(%s) {\n%s\n}" id ps (blk_str "\n" (exp_str 1) exps)
let prog_str funcs =
    String.concat ~sep:"\n\n" (List.map funcs ~f:func_str)
