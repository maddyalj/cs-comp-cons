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
    | separated_list(FUNC_SEP, func_wrap) EOF   { $1 }

func_wrap:
    | FUNC func RIGHT_BRACE     { $2 }
    | FUNC func                 { $2 }
    |       func RIGHT_BRACE    { $1 }
    |       func                { $1 }

func:
    | ID LEFT_PAREN params RIGHT_PAREN LEFT_BRACE exp   { ($1, $3, $6) }

exp:
    | LET ID EQUALS exp SEMICOLON exp                               { `Let ($2, $4, $6) }
    | exp SEMICOLON exp                                             { `Seq ($1, $3) }
    | IF LEFT_PAREN exp RIGHT_PAREN LEFT_BRACE exp RIGHT_BRACE
        ELSE LEFT_BRACE exp RIGHT_BRACE                             { `IfElse ($3, $6, $10) }
    | IF LEFT_PAREN exp RIGHT_PAREN LEFT_BRACE exp RIGHT_BRACE      { `If ($3, $6) }
    | WHILE LEFT_PAREN exp RIGHT_PAREN LEFT_BRACE exp RIGHT_BRACE   { `While ($3, $6) }
    | op_exp op op_exp                                              { `Operator ($2, $1, $3) }
    | ID EQUALS op_exp                                              { `Asg ((`Id $1), $3) }
    | RETURN exp                                                    { `Return $2 }
    | ID                                                            { `Deref (`Id $1) }
    | INT                                                           { `Const $1 }
    | exp SEMICOLON                                                 { $1 }

op_exp:
    | op_exp op op_exp  { `Operator ($2, $1, $3) }
    | ID                { `Deref (`Id $1) }
    | INT               { `Const $1 }

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
    separated_list(COMMA, identifier)   { $1 }

identifier:
    ID  { $1 }
