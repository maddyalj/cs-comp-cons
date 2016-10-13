%token <int> INT
%token <string> ID
%token
    LPAREN RPAREN LBRACE RBRACE
    COMMA SEMICOLON
    IF ELSE WHILE
    CONST LET EQUALS
    PRINT RETURN
    PLUS MINUS TIMES DIVIDE
    LTEQ LT GTEQ GT EQ NOTEQ
    AND OR NOT
    EOF

%type <Lang.func> func
%type <Lang.exp> exp
%type <Lang.op> op

%start <Lang.prog> prog
%%

prog:
    | list(func) EOF { $1 }

func:
    | ID LPAREN separated_list(COMMA, ID) RPAREN LBRACE list(exp) RBRACE
                                                        { ($1, $3, $6) }

exp:
    | IF LPAREN exp RPAREN LBRACE list(exp) RBRACE ELSE LBRACE list(exp) RBRACE
                                                       { IfElse ($3, $6, $10) }
    | IF LPAREN exp RPAREN LBRACE list(exp) RBRACE     { If ($3, $6)          }
    | WHILE LPAREN exp RPAREN LBRACE list(exp) RBRACE  { While ($3, $6)       }
    | CONST ID EQUALS exp                              { Const ($2, $4)       }
    | LET ID EQUALS exp                                { Let ($2, $4)         }
    | ID EQUALS exp                                    { Asg ((Id $1), $3)    }
    | exp op exp                                       { Op ($2, $1, $3)      }
    | PRINT exp                                        { Print $2             }
    | RETURN exp                                       { Return $2            }
    | ID LPAREN separated_list(COMMA, exp) RPAREN      { Appl ($1, $3)        }
    | ID                                               { Deref (Id $1)        }
    | INT                                              { Val $1               }
    | exp SEMICOLON                                    { $1                   }

op:
    | PLUS   { Plus   }
    | MINUS  { Minus  }
    | TIMES  { Times  }
    | DIVIDE { Divide }
    | LTEQ   { Lteq   }
    | LT     { Lt     }
    | GTEQ   { Gteq   }
    | GT     { Gt     }
    | EQ     { Eq     }
    | NOTEQ  { Noteq  }
    | AND    { And    }
    | OR     { Or     }
    | NOT    { Not    }
