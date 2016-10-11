%token <int> INT
%token <string> ID
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token SEMICOLON
%token COMMA
%token TRUE
%token FALSE
%token NULL
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token LEQ
%token GEQ
%token EQUAL
%token NOTEQ
%token AND
%token OR
%token NOT
%token EQUALS
%token LET
%token RETURN
%token FUNC
%token IF
%token ELSE
%token WHILE
%token FUNC_SEP
%token EOF

%start <Lang.program> top
%%

top:
    | fl = separated_list(FUNC_SEP, func_wrap); EOF { fl }
;

func_wrap:
    | FUNC; f = func; RIGHT_BRACE { f }
    | FUNC; f = func              { f }
    |       f = func; RIGHT_BRACE { f }
    |       f = func              { f }

func:
    | i = ID; LEFT_PAREN; p = params; RIGHT_PAREN; LEFT_BRACE; e = exp
                                                                 { (i, p, e) }
;

/*exp_wrap:
    | e = exp;
    | e = exp; SEMICOLON;*/

exp:
    | LET; i = ID; EQUALS; e1 = exp; SEMICOLON; e2 = exp { `Let (i, e1, e2) }
    | e1 = exp; SEMICOLON; e2 = exp                              { `Seq (e1, e2) }
    | IF; LEFT_PAREN; e1 = exp; RIGHT_PAREN;
        LEFT_BRACE; e2 = exp; RIGHT_BRACE;
      ELSE;
        LEFT_BRACE; e3 = exp; RIGHT_BRACE                        { `IfElse (e1, e2, e3) }
    | IF; LEFT_PAREN; e1 = exp; RIGHT_PAREN;
        LEFT_BRACE; e2 = exp; RIGHT_BRACE                        { `If (e1, e2) }
    | WHILE; LEFT_PAREN; e1 = exp; RIGHT_PAREN;
        LEFT_BRACE; e2 = exp; RIGHT_BRACE                        { `While (e1, e2) }
    | e1 = op_exp; o = op; e2 = op_exp                           { `Operator (o, e1, e2) }
    | i = ID; EQUALS; e = op_exp                         { `Asg ((`Id i), e) }
    | RETURN; e = exp                                            { `Return e }
    | s = ID                                             { `Deref (`Id s) }
    | i = INT                                                    { `Const i }
    | e = exp; SEMICOLON { e }
;

op_exp:
    | e1 = op_exp; o = op; e2 = op_exp                           { `Operator (o, e1, e2) }
    | s = ID                                             { `Deref (`Id s) }
    | i = INT                                                    { `Const i }
;

op:
    | PLUS   { `Plus   }
    | MINUS  { `Minus  }
    | TIMES  { `Times  }
    | DIVIDE { `Divide }
    | LEQ    { `Leq    }
    | GEQ    { `Geq    }
    | EQUAL  { `Equal  }
    | NOTEQ  { `Noteq  }
    | AND    { `And    }
    | OR     { `Or     }
    | NOT    { `Not    }

params:
    l = separated_list(COMMA, identifier)   { l }
;

identifier:
    i = ID                          { i }
;
