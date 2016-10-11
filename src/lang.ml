type opcode = [
    | `Plus
    | `Minus
    | `Times
    | `Divide
    | `Leq
    | `Geq
    | `Equal
    | `Noteq
    | `And
    | `Or
    | `Not
]
type expression = [
  | `Seq of expression * expression (* e; e *)
  | `While of expression * expression (* while e do e *)
  | `If of expression * expression (* if e do e *)
  | `IfElse of expression * expression * expression (* if e do e else e *)
  | `Asg of expression * expression (* e := e *)
  | `Deref of expression (* !e *)
  | `Operator of opcode * expression * expression (* e + e *)
  | `Appl of expression * expression (* e(e) *)
  | `Return of expression (* e(e) *)
  | `Const of int (* 7 *)
  | `Id of string (* x *)
  | `Let of string * expression * expression (* let x = e in e *)
]
type fundef = string * string list * expression
type program = fundef list

open Core.Std
let string_of_op = function
    | `Plus   -> "+"
    | `Minus  -> "-"
    | `Times  -> "*"
    | `Divide -> "/"
    | `Leq    -> "<="
    | `Geq    -> ">="
    | `Equal  -> "=="
    | `Noteq  -> "!="
    | `And    -> "&&"
    | `Or     -> "||"
    | `Not    -> "!"
let indent n = String.make (n * 2) ' '
let rec string_of_exp n =
    let ind = indent n in function
        | `Seq (e1, e2)          -> sprintf "%s;\n%s" (string_of_exp n e1) (string_of_exp n e2)
        | `While (e1, e2)        -> sprintf "%swhile (%s) {\n%s\n%s}" ind (string_of_exp 0 e1) (string_of_exp (n + 1) e2) ind
        | `If (e1, e2)           -> sprintf "%sif (%s) {\n%s\n%s}" ind (string_of_exp 0 e1) (string_of_exp (n + 1) e2) ind
        | `IfElse (e1, e2, e3)   -> sprintf "%sif (%s) {\n%s\n%s} else {\n%s\n%s}" ind (string_of_exp 0 e1) (string_of_exp (n + 1) e2) ind (string_of_exp (n + 1) e3) ind
        | `Asg (e1, e2)          -> sprintf "%s%s = %s" ind (string_of_exp 0 e1) (string_of_exp 0 e2)
        | `Deref e               -> sprintf "%s%s" ind (string_of_exp n e)
        | `Operator (op, e1, e2) -> sprintf "%s%s %s %s" ind (string_of_exp 0 e1) (string_of_op op) (string_of_exp 0 e2)
        | `Appl (e1, e2)         -> sprintf "%s%s(%s)" ind (string_of_exp 0 e1) (string_of_exp 0 e2)
        | `Return e              -> sprintf "%sreturn %s" ind (string_of_exp 0 e)
        | `Const i               -> sprintf "%s%d" ind i
        | `Id str                -> sprintf "%s%s" ind str
        | `Let (str, e1, e2)     -> sprintf "%slet %s = %s;\n%s" ind str (string_of_exp 0 e1) (string_of_exp n e2)
let string_of_fd (f, p, d) =
    let deff = (string_of_exp 1 d) in
        sprintf "func %s(%s) {\n%s\n}" f (String.concat ~sep:", " p) deff
let string_of_prog p = String.concat ~sep:"\n\n" (List.map p ~f:string_of_fd)
