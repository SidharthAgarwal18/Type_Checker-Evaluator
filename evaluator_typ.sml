structure EVALUATOR  =
struct
open AST

val mismatch = Fail "Type mismatch in your evaluation"

val gLoBAl = ref ([]:environment);
val gLoBAl2 = ref ([]:typ_environ);

fun typToString(t:typ):string = 
	case t of
	Type INT		=>"int"
	|Type BOOL		=>"bool"
	|CurrType(t1,t2)	=>typToString(t1)^"->"^typToString(t2)
	|ParType t1		=>"("^typToString(t1)^")"
	
fun binopToString(b:binop):string = 
	case b of
	AND		=>"AND"
	|OR		=>"OR"
	|XOR		=>"XOR"
	|EQUALS	=>"EQUALS"
	|IMPLIES	=>"IMPLIES"
	|PLUS		=>"PLUS"
	|MINUS		=>"MINUS"
	|TIMES		=>"TIMES"
	|LESSTHAN	=>"LESSTHAN"
	|GREATERTHAN	=>"GREATERTHAN"

fun uniopToString(u:uniop):string = 
	case u of
	NEGATE		=>"NEGATE"
	|NOT		=>"NOT"
	

fun expToString(e:exp):string =
	case e of 
	NumExp i            	  			=> Int.toString(i)
      | BoolExp b	   	  			=> Bool.toString(b)
      | VarExp x 	    	  			=> x
      | BinExp (b,e1,e2)   	  			=> expToString(e1)^" "^binopToString(b)^" "^expToString(e2)
      | UniExp (u,e1)        	  			=> uniopToString(u)^" "^expToString(e1)
      | LetExp (ValDecl(x,e1),e2) 			=> "let "^x^" = "^expToString(e1)^" in "^expToString(e2)^" end "
      | LetExp (FuncDecl (Fun (i1,i2,t1,t2,e1)),e2)   	=> "let func "^i1^" in "^expToString(e2)^" end "
      | IfExp (e1,e2,e3) 	  			=> " if "^expToString(e1)^" then "^expToString(e2)^" else "^expToString(e3)
      | ParExp e1	    	  			=> expToString(e1)
      | FunExp (e1,e2)		  			=> "("^expToString(e1)^","^expToString(e2)^")"
      | Fn (i1,t1,t2,e1)	 			=> "Fn ("^i1^":"^typToString(t1)^"):"^typToString(t2)^" => "^expToString(e1)

fun retType(v:value):typ = case v of
	IntVal i 			=> Type INT
	|BoolVal b			=> Type BOOL
	|FnVal (i1,t1,t2,e,loc_env)	=> CurrType(t1,t2)
	|FunVal (i1,i2,t1,t2,e)	=> CurrType(t1,t2)


fun compTyp(name:id,t1:typ,t2:typ):bool = 
	case (t1,t2) of
	(Type INT,Type INT)			=>true
	|(Type BOOL,Type BOOL)  		=>true
	|(CurrType (a1,a2),CurrType (b1,b2))	=>(compTyp(name,a1,b1)) andalso (compTyp(name,a2,b2))
	|(ParType a1,_)			=> compTyp(name,a1,t2)
	|(_, ParType b1)			=> compTyp(name,t1,b1)
	| _					=> raise Fail ("Type mismatch in evaluation of "^name^" \n")	

fun midcompTyp(name:id,t1:typ,t2:typ):bool = compTyp(name^typToString(t1)^" actual: "^typToString(t2),t1,t2)

fun checkFuncExp(name:string,t1:typ,t2:typ):typ = 
	case (t1,t2) of
	(Type INT, _ )		=> raise Fail ("argument passed to some non-callable expression of typ: int in exp :"^name)
	|(Type BOOL, _ )	=> raise Fail ("argument passed to some non-callable expression of typ: bool in exp :"^name)
	|(CurrType (a1,a2), b1)=>(compTyp(name^" function expected input type "^typToString(a1)^" actual type "^typToString(b1),a1,b1);
				 a2)
	|(ParType a1, _ )	=>  checkFuncExp(name,a1,t2)
	

and checkIfExp(e1:exp,e2:exp,e3:exp,typ_env:typ_environ):typ =
	case (checkexp(e1,typ_env),checkexp(e2,typ_env),checkexp(e3,typ_env)) of
	(Type BOOL,Type BOOL,Type BOOL)	=> Type BOOL
	|(Type BOOL,Type INT,Type INT)		=> Type INT
	|(Type BOOL,t1,t2)	
=>(compTyp(expToString(IfExp(e1,e2,e3))^" with typ after \"then\" "^typToString(t1)^" and after \"else\" "^typToString(t2),t1,t2);t1)
	|_					=> raise Fail ("Expressions is of type :int after if in "^expToString(IfExp(e1,e2,e3)))


and checkUniExp(u:uniop,e:exp,typ_env:typ_environ):typ =
	case (u,checkexp(e,typ_env)) of
	(NOT,Type BOOL)	=> Type BOOL
	|(NEGATE,Type INT)	=> Type INT
	| _ 		=> raise Fail ("Type mismatch in "^expToString(UniExp(u,e))^" with typ "^typToString(checkexp(e,typ_env)))


and checkBinExp(b:binop, e1:exp, e2:exp,typ_env:typ_environ):typ = 
	case (b, checkexp(e1,typ_env),checkexp(e2,typ_env))  of
	(AND,Type BOOL,Type BOOL) 		=> Type BOOL
	|(OR,Type BOOL,Type BOOL) 		=> Type BOOL
	|(XOR,Type BOOL,Type BOOL)		=> Type BOOL
	|(EQUALS,Type BOOL,Type BOOL) 		=> Type BOOL
	|(EQUALS,Type INT,Type INT)	   	=> Type BOOL
	|(IMPLIES,Type BOOL,Type BOOL)		=> Type BOOL
	|(PLUS,Type INT,Type INT) 		=> Type INT
	|(MINUS,Type INT,Type INT)		=> Type INT
	|(TIMES,Type INT,Type INT)		=> Type INT
	|(LESSTHAN,Type INT,Type INT) 		=> Type BOOL
	|(GREATERTHAN, Type INT, Type INT) 	=> Type BOOL
	| _	=> raise Fail ("Type mismatch in "^expToString(BinExp(b,e1,e2))^" with typ1 "^typToString(checkexp(e1,typ_env))^" with typ2 "^typToString(checkexp(e2,typ_env)))


and checkexp(e:exp,typ_env:typ_environ):typ = 
	case e of
	NumExp i            	  => Type INT
      | BoolExp b	   	  => Type BOOL
      | VarExp x 	    	  => envLookup(x,typ_env,gLoBAl2)
      | BinExp (b,e1,e2)   	  => checkBinExp(b,e1,e2,typ_env)
      | UniExp (u,e1)        	  => checkUniExp(u,e1,typ_env)
      | LetExp (ValDecl(x,Fn (i1,t1,t2,e1)),e2)=> let val v1 = checkexp(Fn (i1,t1,t2,e1),envAdd(x,CurrType(t1,t2),typ_env)) in
     (compTyp("lambda function "^x^" expected typ "^typToString(CurrType(t1,t2))^" actual "^typToString(v1),v1,CurrType(t1,t2)); checkexp(e2,envAdd(x,v1,typ_env))) end
      | LetExp (ValDecl(x,e1),e2) => let val v1 = checkexp(e1,typ_env) in checkexp(e2, envAdd (x, v1, typ_env)) end
      | LetExp (FuncDecl f,e2)    => checkexp(e2,checkFunc(f,typ_env))
      | IfExp (e1,e2,e3) 	  => checkIfExp(e1,e2,e3,typ_env)
      | ParExp e1	    	  => checkexp(e1,typ_env)
      | FunExp (VarExp e1,e2)	  => checkFuncExp("function call of "^e1^",",checkexp(VarExp e1,typ_env),checkexp(e2,typ_env)) 
      | FunExp (e1,e2)		  => checkFuncExp(expToString(FunExp(e1,e2)),checkexp(e1,typ_env),checkexp(e2,typ_env))
      | Fn (i1,t1,t2,e1)	  => 
 (midcompTyp(expToString(Fn (i1,t1,t2,e1))^" return type expected: ",t2,checkexp(e1,envAdd(i1,t1,typ_env)));CurrType(t1,t2))
	

and checkFunc(Fun (i1,i2,t1,t2,e),typ_env):typ_environ=
(midcompTyp("function named "^i1^" return type expected: ",t2,checkexp(e,envAdd(i1,CurrType(t1,t2),envAdd(i2,t1,typ_env))));
		envAdd(i1,CurrType(t1,t2),typ_env))

and checkState(s:state,env:typ_environ):typ =
	case s of
	Exp e1			=>(let val v1 = checkexp(e1,env) in 
				   case v1 of
				   Type INT	=> Type INT
				   |Type BOOL	=> Type BOOL
				   | _ 	=> raise Fail ("Final return type not int or bool expression in"^expToString(e1)) end)
	|Function f		=>(gLoBAl2 := checkFunc(f,!gLoBAl2); Type INT)


and checkAST(p:pro) = 
	case p of
	UniState (s)		=>(checkState(s,[]);"Type checking successfull")
	|MultiState (s,p1) 	=>(checkState(s,[]);checkAST(p1))

and evalAST(p:pro):result = let val p2 = p in 
	(checkAST(p2); gLoBAl2 := [];(*TextIO.output(TextIO.stdOut,"type checking");*)
	evaluateAST(p)) end

and evaluateAST(p:pro):result = 
	case p of
	UniState (s)		=>let val v2 = [(evalState(s,[]))] in (gLoBAl := []; v2) end
	|MultiState (s,p1) 	=>(evalState(s,[])::evaluateAST(p1)) 

and declareFunc(Fun (i1,i2,t1,t2,e),env:environment):environment = envAdd(i1,FunVal (i1,i2,t1,t2,e),env)
	
and evalState(s:state,env:environment):value =
	case s of
	Exp e1					=> evalExp(e1,env)
	|Function (Fun (i1,i2,t1,t2,e))	=>(gLoBAl := declareFunc((Fun (i1,i2,t1,t2,e)),!gLoBAl); envLookup(i1,env,gLoBAl))

and evalFunc(var:value,v:value,env:environment):value = 
	case var of
	FnVal (i1,t1,t2,e,loc_env)      => (compTyp("lambda function of "^i1,t1,retType(v)); 
					    (case e of 
			Fn (x1,y1,y2,z1) =>(compTyp("lambda function of "^x1,t2,CurrType(y1,y2));FnVal (x1,y1,y2,z1,(i1,v)::loc_env))
					|_		 => (let val v1 = evalExp(e,envAdd(i1,v,loc_env@env)) in 
								compTyp("lambda epxression of "^i1,t2,retType(v1)); v1 end)
					   ))
	|FunVal (i1,i2,t1,t2,e)	=> (compTyp("function named "^i1,t1,retType(v));
					  let val v1 = evalExp(e,envAdd(i2,v,env)) in compTyp("function named "^i1,t2,retType(v1));v1 end
					    )
	| _				=> raise Fail "argument passed to some non-callable expression of typ: int or bool"
	
and evalExp(e:exp, env:environment):value =
    case e of
	NumExp i            	  => IntVal i
      | BoolExp b	    	  => BoolVal b
      | VarExp x 	   	  => envLookup(x,env,gLoBAl)
      | BinExp (b,e1,e2)    	  => evalBinExp(b, e1, e2, env)
      | UniExp (u,e1)       	  => evalUniExp(u,e1,env)
      | LetExp (ValDecl(x,e1),e2) => let val v1 = evalExp (e1, env) in evalExp(e2, envAdd (x, v1, env)) end
      | LetExp (FuncDecl f,e2)    => evalExp(e2,declareFunc(f,env))
      | IfExp (e1,e2,e3) 	  => evalIfExp(e1,e2,e3,env)
      | ParExp e1	    	  => evalExp(e1,env)
      | FunExp (e1,e2)	  	  => evalFunc(evalExp(e1,env),evalExp(e2,env),env) (*evaluation using call by value strategy*)
      | Fn (i1,t1,t2,e1)	  => FnVal (i1,t1,t2,e1,[])
           	   
and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
	if (b=OR) then (if (evalExp(e1,env)=BoolVal (true)) then BoolVal (true) else evalExp(e2,env)) 
	else if(b=IMPLIES) then (if (evalExp(e1,env)=BoolVal (false)) then BoolVal(true) else evalExp(e2,env))
	else if(b=AND) then (if (evalExp(e1,env)=BoolVal (false)) then BoolVal(false) else evalExp(e2,env))
	else(
	case (b, evalExp(e1, env), evalExp(e2, env))  of
	(XOR,BoolVal b1, BoolVal b2)		=> BoolVal ((b1 orelse b2) andalso not(b1 andalso b2))
	|(EQUALS,BoolVal b1,BoolVal b2) 	=> BoolVal (if (b1=b2) then true else false)
	|(EQUALS,IntVal i1,IntVal i2)   	=> BoolVal (if (i1=i2) then true else false)
	|(PLUS,IntVal i1,IntVal i2) 		=> IntVal (i1+i2)
	|(MINUS,IntVal i1,IntVal i2)		=> IntVal (i1-i2)
	|(TIMES,IntVal i1,IntVal i2)		=> IntVal (i1*i2)
	|(LESSTHAN,IntVal i1,IntVal i2) 	=> BoolVal (i1 < i2)
	|(GREATERTHAN, IntVal i1, IntVal i2) 	=> BoolVal (i1 > i2)
	| _				     	=> raise Fail ("Type mismatch in a binary expression "^expToString(BinExp(b,e1,e2))))

and evalUniExp(u:uniop,e:exp,env:environment):value = 
	case (u,evalExp(e,env)) of
	(NOT,BoolVal b)			=> BoolVal (not b)   
	|(NEGATE,IntVal i)			=> IntVal (~1 * i)
	| _					=> raise Fail ("Type mismatch in a unary expression "^expToString(UniExp(u,e)))

and evalIfExp(e1:exp,e2:exp,e3:exp,env:environment):value =
		if(evalExp(e1,env)= BoolVal (true)) then evalExp(e2,env) else evalExp(e3,env)
						    
end
