%{
    open Lang
    let some = function
        | None   -> Empty
        | Some e -> e
%}

%token <int> INT
%token <string> ID
%token
    LPAREN RPAREN LBRACE RBRACE
    COMMA SEMICOLON
    IF ELSE WHILE
    CONST LET EQUALS PRINT
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
    | func* EOF { $1 }

func:
    | ID LPAREN separated_list(COMMA, ID) RPAREN LBRACE exp? RBRACE
                                              { ($1, $3, some $6) }

exp:
    | LPAREN exp RPAREN                                              { $2 }
    | IF LPAREN exp RPAREN LBRACE exp? RBRACE ELSE LBRACE exp? RBRACE
                                         { IfElse ($3, some $6, some $10) }
    | IF LPAREN exp RPAREN LBRACE exp? RBRACE     { If ($3, some $6)      }
    | WHILE LPAREN exp RPAREN LBRACE exp? RBRACE  { While ($3, some $6)   }
    | CONST ID EQUALS exp SEMICOLON exp           { Const ($2, $4, $6)    }
    | CONST ID EQUALS exp                         { Const ($2, $4, Empty) }
    | LET ID EQUALS exp SEMICOLON exp             { Let ($2, $4, $6)      }
    | LET ID EQUALS exp                           { Let ($2, $4, Empty)   }
    | ID EQUALS exp                               { Asg ((Id $1), $3)     }
    | exp op exp                                  { Op ($2, $1, $3)       }
    | PRINT exp                                   { Print $2              }
    | ID LPAREN separated_list(COMMA, exp) RPAREN { Appl ($1, $3)         }
    | ID                                          { Deref (Id $1)         }
    | INT                                         { Val $1                }
    | exp SEMICOLON exp                           { Seq ($1, $3)          }
    | exp SEMICOLON                               { $1                    }
    | exp exp                                     { Seq ($1, $2)          }

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
