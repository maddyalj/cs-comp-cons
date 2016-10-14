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
    | IfElse of exp * exp * exp   (* if (e) {es} else {es} *)
    | If     of exp * exp         (* if (e) {es} *)
    | While  of exp * exp         (* if (e) {es} *)
    | Const  of string * exp      (* const x = e *)
    | Let    of string * exp      (* let x = e *)
    | Asg    of exp * exp         (* e = e *)
    | Op     of op * exp * exp    (* e + e *)
    | Print  of exp               (* print e *)
    | Return of exp               (* return e *)
    | Appl   of string * exp list (* foo(es) *)
    | Deref  of exp               (* !e *)
    | Val    of int               (* 7 *)
    | Id     of string            (* x *)
    | Seq    of exp * exp         (* e; e *)
    | Empty                       (* *)
type func = string * string list * exp
type prog = func list
