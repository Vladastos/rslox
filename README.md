# Rslox

Rslox is a Lox interpreter written in Rust. It is a simple interpreter for the [Lox](https://github.com/munificent/craftinginterpreters) programming language.

## Language Specification

The language specification for Lox can be found [here](https://craftinginterpreters.com/).

Lox is a small, simple language with a small set of keywords and operators. Its syntax is similar to C++ and Java, but with a few differences.


## Usage

```bash
$ cargo run
```

## Grammar 

Here is the grammar that is implemented for now by Rslox. It will be updated as more of the language is implemented.

>---
> **Program** -> Declaration\* EOF
>
>**Declaration** -> VarDeclaration | Statement
>
>**VarDeclaration** -> "var" *IDENTIFIER* ( "=" Expression )? ;
>
>**Statement** -> ExpressionStatement | PrintStatement | Block
>
>**Block** -> "{" Declaration\* "}"
>
>**ExpressionStatement** -> Expression ;
>
>**PrintStatement** -> "print" Expression ;
>
>**Expression** -> AssignmentExpression
>
>**AssignmentExpression** -> *IDENTIFIER* "=" AssignmentExpression | LogicalOrExpression 
>
>**LogicalOrExpression** -> LogicalAndExpression ( "||" LogicalAndExpression )\*
>
>**LogicalAndExpression** -> ComparisonExpression ( "&&" ComparisonExpression )\*
>
>**ComparisonExpression** -> AdditionExpression ( ( ">" | ">=" | "<" | "<=" | "!=" | "==" ) AdditionExpression )
>
>**AdditionExpression** -> MultiplicationExpression ( ( "+" | "-" ) MultiplicationExpression )\*
>
>**MultiplicationExpression** -> UnaryExpression ( ( "\*" | "/" )UnaryExpression )\*
>
>**UnaryExpression** -> ( "!" | "-" ) UnaryExpression | PrimaryExpression
>
>**PrimaryExpression** -> *NUMBER* | *STRING* | "true" | "false" | "nil" | "(" Expression ")" | *IDENTIFIER*
>
>---