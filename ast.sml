structure AST =
struct

type id = string

datatype binop = AND|OR|XOR|EQUALS|IMPLIES|PLUS|MINUS|TIMES|LESSTHAN|GREATERTHAN

datatype tYPE = BOOL|INT

datatype paren = LPAREN|RPAREN

datatype uniop = NOT|NEGATE

datatype condition = IF|ELSE|THEN|FI

datatype typ = CurrType of typ*typ
	       |ParType of typ
	       |Type of tYPE
	       
datatype pro = UniState of state
	  |MultiState of state*pro
	  
and state = Exp of exp
	    |Function of func
	    
and decl = ValDecl of id*exp
	   |FuncDecl of func
	   	    
and exp =   	NumExp of int
		|BoolExp of bool
		|VarExp of id
		|BinExp of binop*exp*exp
		|UniExp of uniop*exp
		|LetExp of decl*exp
		|IfExp of exp*exp*exp
		|ParExp of exp
		|FunExp of exp*exp
		|Fn of id*typ*typ*exp
	
and func =  Fun of id*id*typ*typ*exp

datatype value = IntVal of int|BoolVal of bool|FnVal of id*typ*typ*exp*((id*value)list)|FunVal of id*id*typ*typ*exp

type environment = (id*value) list

type typ_environ = (id*typ) list

type result = (value)list

fun envAdd (var:id, v, env) = (var,v)::env

fun globalLookup (var:id, global) =
    case List.find(fn (x, _) => x = var) (!global) of
				       SOME (x, v)   => v
				    |   NONE => raise Fail ("Environment lookup error for variable \""^var^"\"")					    

fun envLookup (var:id, env, global) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => globalLookup(var,global)					    
end
	

