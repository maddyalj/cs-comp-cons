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
%left OR
%left AND
%left LTEQ LT GTEQ GT EQ NOTEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left NOT

%type <Lang.func> func
%type <Lang.exp> exp
%type <Lang.op> op

%start <Lang.prog> prog
%%

prog:
    | func* EOF { $1 }

func:
    | ID LPAREN separated_list(COMMA, ID) RPAREN LBRACE seq_exp RBRACE
                                                      { ($1, $3, $6) }

seq_exp:
    | CONST ID EQUALS exp SEMICOLON seq_exp { Const ($2, $4, $6)    }
    | CONST ID EQUALS exp SEMICOLON         { Const ($2, $4, Empty) }
    | LET ID EQUALS exp SEMICOLON seq_exp   { Let ($2, $4, $6)      }
    | LET ID EQUALS exp SEMICOLON           { Let ($2, $4, Empty)   }
    | exp SEMICOLON seq_exp                 { Seq ($1, $3)          }
    | exp seq_exp                           { Seq ($1, $2)          }
    | exp SEMICOLON?                        { $1                    }

exp:
    | LPAREN seq_exp RPAREN                         { $2                    }
    | IF LPAREN exp RPAREN LBRACE seq_exp RBRACE ELSE LBRACE seq_exp RBRACE
                                                    { If ($3, $6, $10)      }
    | WHILE LPAREN exp RPAREN LBRACE seq_exp RBRACE { While ($3, $6)        }
    | CONST ID EQUALS exp SEMICOLON seq_exp         { Const ($2, $4, $6)    }
    | CONST ID EQUALS exp                           { Const ($2, $4, Empty) }
    | LET ID EQUALS exp SEMICOLON seq_exp           { Let ($2, $4, $6)      }
    | LET ID EQUALS exp                             { Let ($2, $4, Empty)   }
    | ID EQUALS exp                                 { Asg ((Id $1), $3)     }
    | PRINT exp                                     { Print $2              }
    | ID LPAREN separated_list(COMMA, exp) RPAREN   { Appl ($1, $3)         }
    | ID                                            { Deref (Id $1)         }
    | exp EQUALS exp                                { Asg ($1, $3)          }
    | INT                                           { Val $1                }
    | exp op exp                                    { Op ($2, $1, $3)       }
    | NOT exp                                       { Op (Not, $2, Empty)   }

%inline op:
    | PLUS   { Plus : Lang.op   }
    | MINUS  { Minus : Lang.op  }
    | TIMES  { Times : Lang.op  }
    | DIVIDE { Divide : Lang.op }
    | LTEQ   { Lteq : Lang.op   }
    | LT     { Lt : Lang.op     }
    | GTEQ   { Gteq : Lang.op   }
    | GT     { Gt : Lang.op     }
    | EQ     { Eq : Lang.op     }
    | NOTEQ  { Noteq : Lang.op  }
    | AND    { And : Lang.op    }
    | OR     { Or : Lang.op     }
