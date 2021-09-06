structure Tokens = Tokens

  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  
  	val pos = ref 1
	val col = ref 0
	val dummy1 = ref 1
	val comma = ref 0
	val dummy2 = ref 1
	val fine = ref 1
	val eof = fn ()=> (dummy1 := !pos;dummy2 := !col;pos :=1; col := 0;comma := 0;Tokens.EOF(!dummy1,!dummy2))
	val error = fn (x,i:int,j:int) => 
	(fine := 0;TextIO.output(TextIO.stdOut,"Unknown token:"^Int.toString(i)^":"^Int.toString(j)^":"^x^"\n"))
	val checker = fn (x) => if !x=1 then TextIO.output(TextIO.stdOut,",") else x:=1
	
	fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  %%
%header (functor AlgebraLexFun(structure Tokens:Algebra_TOKENS));

alpha = [A-Za-z];
alpha_dig = [A-Za-z0-9];
digit = [0-9];
ws = [\ \t \n];
%%

{ws}+ => (lex());
{digit}+ => (checker(comma); col := !col + 1; TextIO.output(TextIO.stdOut,"NUM \"NUM\" "); Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !pos, !col));
":" =>(checker(comma); col := !col + 1; TextIO.output(TextIO.stdOut,"COLON \"COLON\" ");Tokens.COLON(!pos,!col));
"->" =>(checker(comma); col := !col + 1;TextIO.output(TextIO.stdOut,"ARROW \"ARROW\" ");Tokens.ARROW(!pos,!col));
"=>"=>(checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"F_ARROW \"F_ARROW\" ");Tokens.FARROW(!pos,!col));
"=" => (checker(comma); col := !col + 1; TextIO.output(TextIO.stdOut,"NUM \"NUM\" ");Tokens.EQ(!pos,!col)); 
";" => (checker(comma); dummy1:= !col ;col := 0; pos := !pos +1; TextIO.output(TextIO.stdOut,"TERM \";\" "); Tokens.TERM(!pos,!dummy1));
"int" => (checker(comma); dummy1:= !col ;col := 0; pos := !pos +1; TextIO.output(TextIO.stdOut,"INT \"INT\" "); Tokens.INT(!pos,!dummy1));
"bool"=>(checker(comma);col := !col + 1; TextIO.output(TextIO.stdOut,"BOOL \"BOOL\" "); Tokens.BOOL(!pos,!dummy1));
"fn" =>(checker(comma); col := !col + 1; TextIO.output(TextIO.stdOut,"FN \"FN\" "); Tokens.FN(!pos,!dummy1));
"fun"=>(checker(comma); col := !col + 1; TextIO.output(TextIO.stdOut,"FUN \"FUN\" "); Tokens.FUN(!pos,!dummy1));
"AND" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"AND \"AND\" "); Tokens.AND(!pos,!col));
"OR" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"OR \"OR\" "); Tokens.OR(!pos,!col));
"XOR" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"XOR \"XOR\" "); Tokens.XOR(!pos,!col));
"EQUALS" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"EQUALS \"EQUALS\" "); Tokens.EQUALS(!pos,!col));
"TRUE" =>(checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"CONST \"TRUE\" "); Tokens.TRUE(!pos,!col));
"FALSE" =>(checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"CONST \"FALSE\" "); Tokens.FALSE(!pos,!col));
"NOT" =>(checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"NOT \"NOT\" "); Tokens.NOT(!pos,!col));
"IMPLIES" =>(checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"IMPLIES \"IMPLIES\" "); Tokens.IMPLIES(!pos,!col));
"if" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"IF \"if\" "); Tokens.IF(!pos,!col));
"else" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"ELSE \"else\" "); Tokens.ELSE(!pos,!col));
"then" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"THEN \"then\" "); Tokens.THEN(!pos,!col));
"fi" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"FI \"fi\" ");Tokens.FI(!pos,!col));
"PLUS" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"PLUS \"PLUS\" ");Tokens.PLUS(!pos,!col));
"MINUS" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"MINUS \"MINUS\" ");Tokens.MINUS(!pos,!col));
"TIMES" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"TIMES \"TIMES\" ");Tokens.TIMES(!pos,!col));
"NEGATE" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"NEGATE \"NEGATE\" ");Tokens.NEGATE(!pos,!col));
"LESSTHAN" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"LESSTHAN \"LESSTHAN\" ");Tokens.LESSTHAN(!pos,!col));
"GREATERTHAN" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"GREATERTHAN \"GREATERTHAN\" ");Tokens.GREATERTHAN(!pos,!col));
"let" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"LET \"let\" ");Tokens.LET(!pos,!col));
"in" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"IN \"in\" ");Tokens.IN(!pos,!col));
"end" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"END \"end\" ");Tokens.END(!pos,!col));
"(" =>(checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"LPAREN \"(\" "); Tokens.LPAREN(!pos,!col));
")" => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"RPAREN \")\" "); Tokens.RPAREN(!pos,!col));
{alpha} => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"ID \""^yytext^"\" "); Tokens.ID(yytext,!pos,!col));
{alpha}{alpha_dig}+ => (checker(comma);col := !col + 1;TextIO.output(TextIO.stdOut,"ID \""^yytext^"\" "); Tokens.ID(yytext,!pos,!col));
. =>(error(yytext,!pos,!col+1); comma := 0;
	dummy1 := !pos; dummy2 := !col;
	pos:=1; col := 0;
	Tokens.EOF(!dummy1,!dummy2+1));

