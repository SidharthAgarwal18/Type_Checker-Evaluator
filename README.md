#Type checker and Evaluator

## Boolean and Airthmetic expressions
* This is a program for lexing, parsing, type-checking and evaluation of expression trees generated,for a custom functional langauge
containing only boolean and airthmetic expressions.
* I have implemented along with it function abstraction, function call using call by value strategy, recursive functions and local scoped definitions.

## Instructions
* If `.yacc.sig`, `.yacc.desc`, `.yacc.sml` is not present use `$ ml-yacc "parser.yacc";`
* If `.lex.sml` is not present use `$ ml-lex "lexer.lex";`
* use `$ rlwrap sml`
* In SML environment type `use "compiler.sml";`
* For displaying the lexing and AST Tree for the program saved in <filename> use `lex_parse("<filename>");`
* For first type checking and then evaluating the AST Tree `evalAST(<AST_Tree>);`
* For just type checking use `checkAST(<AST_Tree>);`

## Notes
* evalAST will return a list of values corresponding to each statement, where for a function declaration (as a statement) it will return 
the AST tree for the function expression, for an expression it will return the value evaluated for that expression.
* checkAST will return the string "Type checking is successfull" if everything goes fine.
* both evalAST and checkAST will raise error for any type-mismatch with appropriately locating the cause of error.

## Error Handling
* A lexing error will be displayed mentioning the unkown Token
* Parsing error will be displayed appropriately
* Environment lookup error will be displayed along the id raising the exception.
* Type checking will be done prior to the evaluation and will display type mismatch appropriately.
