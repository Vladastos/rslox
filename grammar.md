# Grammar
This is the grammar of Yoloscript. It is still very much a work in progress, so it may change in the future.

## EBNF

Program              ::= { Declaration } EOF ;

Declaration          ::= ConstDeclaration
                       | VarDeclaration    
                       | FunDeclaration
                       | Statement ;

ConstDeclaration     ::= "let" IDENTIFIER  [ "=" Expression ] ";" ;

VarDeclaration       ::= "mut" IDENTIFIER [ "=" Expression ] ";" ;

FunDeclaration       ::= "fun" IDENTIFIER "(" [ Parameters ] ")" BlockStatement ;

Parameters           ::= IDENTIFIER { "," IDENTIFIER } ;

Statement            ::= ExpressionStatement
                       | PrintStatement
                       | WhileStatement
                       | IfStatement
                       | ForStatement
                       | ReturnStatement
                       | BlockStatement ;

WhileStatement       ::= "while" "(" Expression ")" Statement ;

ForStatement         ::= "for" "(" [ Expression ] ";" [ Expression ] ";" [ Expression ] ")" Statement ;

IfStatement          ::= "if" "(" Expression ")" Statement
                         [ "else" Statement ] ;

BlockStatement       ::= "{" { Declaration } "}" ;

ExpressionStatement  ::= Expression ";" ;

ReturnStatement      ::= "return" [ Expression ] ";" ;

PrintStatement       ::= "print" Expression ";" ;

Expression           ::= AssignmentExpression ;

AssignmentExpression ::= Assignment
                       | LogicalOrExpression ;

Assignment           ::= IDENTIFIER "=" Expression ;

LogicalOrExpression  ::= LogicalAndExpression
                       { "||" LogicalAndExpression } ;

LogicalAndExpression ::= ComparisonExpression
                       { "&&" ComparisonExpression } ;

ComparisonExpression ::= AdditionExpression
                       { ( ">" | ">=" | "<" | "<=" | "!=" | "==" )
                         AdditionExpression } ;

AdditionExpression   ::= MultiplicationExpression
                       { ( "+" | "-" ) MultiplicationExpression } ;

MultiplicationExpression
                     ::= UnaryExpression
                       { ( "*" | "/" ) UnaryExpression } ;

UnaryExpression      ::= ( "!" | "-" ) UnaryExpression
                       | CallExpression ;

CallExpression       ::= PrimaryExpression
                       { "(" [ Arguments ] ")" } ;

Arguments            ::= Expression { "," Expression } ;

PrimaryExpression    ::= NUMBER
                       | STRING
                       | "true"
                       | "false"
                       | "nil"
                       | "(" Expression ")"
                       | IDENTIFIER ;


## Graph

### Program

```mermaid
graph TD
    Program([Program]) -.-> Declaration[  Declaration ]
    Declaration --> OneOf1{" "}


    OneOf1 --> ConstDeclaration
    OneOf1 --> VarDeclaration
    OneOf1 --> FunDeclaration
    OneOf1 --> TypeDeclaration
    OneOf1 --> Statement

```

### Const/Var Declaration

```mermaid
graph TD
    ConstVarDeclaration[["Const/Var Declaration"]] --> EachOf1{{" "}}

    EachOf1 --> Keyword(["#quot;let#quot; #vert; #quot;mut#quot;"])
    EachOf1 --> Identifier["Identifier"]
    EachOf1 --> Colon(["#quot;#colon;#quot;"])
    EachOf1 --> Type["Type Identifier"]
    EachOf1 --> Equals(["#quot;#equals;#quot;"])
    EachOf1 --> Expression["Expression"]
    EachOf1 --> Semicolon(["#quot;;#quot;"])
```

### Function Declaration

```mermaid
graph TD
    FunDeclaration[["Function Declaration"]] --> EachOf{{" "}}
    EachOf--> Keyword(["#quot;fun#quot;"])
    EachOf --> Identifier["Identifier"]
    EachOf --> LeftParen(["#quot;#lpar;#quot;"])
    EachOf --> Parameters["Parameters"]
    EachOf --> RightParen(["#quot;#rpar;#quot;"])
    EachOf --> Colon(["#quot;#colon;#quot;"])
    EachOf --> Type["Type Identifier"]
    EachOf --> BlockStatement["Block Statement"]
```

### Type Declaration

```mermaid
graph TD
    TypeDeclaration[["Type Declaration"]] --> EachOf{{" "}}
    EachOf --> Keyword(["#quot;type#quot;"])
    EachOf --> Identifier["Type Identifier"]
    EachOf --> TypeParameters["Type Parameters"]
    EachOf --> LeftBrace(["#quot;#lbrace;#quot;"])
    EachOf --> TypeDefinition["Type Definition"]
    EachOf --> RightBrace(["#quot;#rbrace;#quot;"])
```
