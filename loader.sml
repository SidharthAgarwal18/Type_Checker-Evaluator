structure AlgebraLrVals = AlgebraLrValsFun(structure Token = LrParser.Token)
structure AlgebraLex = AlgebraLexFun(structure Tokens = AlgebraLrVals.Tokens);
structure AlgebraParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = AlgebraLrVals.ParserData
     	       structure Lex = AlgebraLex)
     	       
fun invoke lexer =
    	     	let fun print_parse_error (s,i:int,j:int) =
		    	TextIO.output(TextIO.stdOut, "Syntax Error:"^(Int.toString i)^":"^(Int.toString j)^":"^s^"]\n")
		in
		    AlgebraParser.parse(0,lexer,print_parse_error,())
		end

fun StringToLexer str =
    let val done = ref false
    	val lexer=  AlgebraParser.makeLexer (fn _ => if (!done) then (TextIO.output(TextIO.stdOut,"]\n");"") else (done:=true;str))
    in
    	TextIO.output(TextIO.stdOut,"[");
    	lexer
    end	
		
fun Parse (lexer) =let 	
			val (result,lexer) = invoke lexer
		   in
		   result
		   end

val ParseString = Parse o StringToLexer

fun lex_parse(infilename) = 
			let 
			val instream = TextIO.openIn infilename
			val file = TextIO.input(instream)
			in
				ParseString file				
			end
open EVALUATOR;		

