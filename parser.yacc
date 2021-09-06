%%
%name Algebra

%term
 TERM|AND|OR|XOR|EQUALS|NUM of int|FALSE
 |TRUE|NOT|IMPLIES|IF|ELSE|FI|EQ
 |THEN|LPAREN|RPAREN|ID of string
 |PLUS|MINUS|TIMES|NEGATE
 |LESSTHAN|GREATERTHAN|LET|IN|END
 |INT|BOOL|ARROW|FARROW|COLON|FN|FUN|EOF
 
%nonterm 
  START of AST.pro |program of AST.pro| statement of AST.state|formula of AST.exp| dec of AST.decl |vartype of AST.typ
  |function of AST.func
    
 %pos int
 
 %eop EOF
 %noshift EOF
 
 %nonassoc FARROW
 %right ARROW
 %right ELSE THEN IF
 %right IMPLIES 
 %left OR XOR AND EQUALS
 %left LESSTHAN GREATERTHAN
 %left PLUS MINUS
 %left TIMES
 %right NOT NEGATE

 
 %start START
 %verbose
 
 %%
 
 START : program(program)
 
 program : statement TERM program (AST.MultiState(statement,program))
 	| statement(AST.UniState(statement))
  
 statement : formula (AST.Exp(formula))| function (AST.Function(function))
 
 dec : ID EQ formula(AST.ValDecl(ID,formula))| function(AST.FuncDecl(function))
 
 formula : NUM(AST.NumExp(NUM))
 	|TRUE(AST.BoolExp(true)) | FALSE(AST.BoolExp(false))
 	|NOT formula(AST.UniExp(AST.NOT,formula))
 	|formula AND formula(AST.BinExp(AST.AND,formula1,formula2))
 	|formula OR formula(AST.BinExp(AST.OR,formula1,formula2))
 	|formula XOR formula(AST.BinExp(AST.XOR,formula1,formula2))
 	|formula EQUALS formula(AST.BinExp(AST.EQUALS,formula1,formula2))
 	|formula IMPLIES formula(AST.BinExp(AST.IMPLIES,formula1,formula2))
 	|IF formula THEN formula ELSE formula FI(AST.IfExp(formula1,formula2,formula3))
 	|LPAREN formula RPAREN(AST.ParExp(formula))
 	|formula PLUS formula(AST.BinExp(AST.PLUS,formula1,formula2))
 	|formula MINUS formula(AST.BinExp(AST.MINUS,formula1,formula2))
 	|formula TIMES formula(AST.BinExp(AST.TIMES,formula1,formula2))
 	|NEGATE formula(AST.UniExp(AST.NEGATE,formula))
 	|formula LESSTHAN formula(AST.BinExp(AST.LESSTHAN,formula1,formula2))
 	|formula GREATERTHAN formula(AST.BinExp(AST.GREATERTHAN,formula1,formula2))
 	|LET dec IN formula END(AST.LetExp(dec,formula))
 	|FN LPAREN ID COLON vartype RPAREN COLON vartype FARROW formula(AST.Fn(ID,vartype1,vartype2,formula))
 	|LPAREN formula formula RPAREN(AST.FunExp(formula1,formula2))
 	|ID(AST.VarExp(ID))
 	
function : FUN ID LPAREN ID COLON vartype RPAREN COLON vartype FARROW formula(AST.Fun(ID1,ID2,vartype1,vartype2,formula))
	   
vartype : INT(AST.Type(AST.INT))
 	|BOOL(AST.Type(AST.BOOL))
 	|vartype ARROW vartype(AST.CurrType(vartype1,vartype2))
 	|LPAREN vartype RPAREN(AST.ParType(vartype))


