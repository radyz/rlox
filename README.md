Usage

```sh
> lox 'source code'
```

```sh
> lox -f './source-code.lx'
```

Grammar

```
expression -> equality ;

equality -> comparison ( ( "!=" | "==" ) comparison )* ;

comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

term -> factor ( ( "-" | "+" ) factor )* ;

factor -> unary ( ( "/" | "*" ) unary )* ;

unary -> ( "!" | "-" ) unary | primary ;

primary -> number | string | "true" | "false" | "nil" | "(" expression ")" ;
```
