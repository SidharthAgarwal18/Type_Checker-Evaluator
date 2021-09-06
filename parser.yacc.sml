functor AlgebraLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Algebra_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\006\000\017\000\007\000\016\000\008\000\015\000\009\000\014\000\
\\010\000\023\000\011\000\013\000\016\000\012\000\017\000\054\000\
\\018\000\011\000\019\000\022\000\020\000\021\000\021\000\020\000\
\\022\000\010\000\023\000\019\000\024\000\018\000\025\000\009\000\
\\033\000\008\000\000\000\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\010\000\023\000\012\000\068\000\019\000\022\000\020\000\021\000\
\\021\000\020\000\023\000\019\000\024\000\018\000\000\000\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\010\000\023\000\013\000\078\000\019\000\022\000\020\000\021\000\
\\021\000\020\000\023\000\019\000\024\000\018\000\000\000\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\010\000\023\000\015\000\055\000\019\000\022\000\020\000\021\000\
\\021\000\020\000\023\000\019\000\024\000\018\000\000\000\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\010\000\023\000\017\000\060\000\019\000\022\000\020\000\021\000\
\\021\000\020\000\023\000\019\000\024\000\018\000\000\000\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\010\000\023\000\019\000\022\000\020\000\021\000\021\000\020\000\
\\023\000\019\000\024\000\018\000\027\000\067\000\000\000\
\\001\000\006\000\017\000\007\000\016\000\008\000\015\000\009\000\014\000\
\\011\000\013\000\016\000\012\000\018\000\011\000\022\000\010\000\
\\025\000\009\000\033\000\008\000\000\000\
\\001\000\006\000\017\000\007\000\016\000\008\000\015\000\009\000\014\000\
\\011\000\013\000\016\000\012\000\018\000\011\000\022\000\010\000\
\\025\000\009\000\033\000\008\000\034\000\007\000\000\000\
\\001\000\014\000\052\000\000\000\
\\001\000\016\000\030\000\000\000\
\\001\000\016\000\049\000\000\000\
\\001\000\016\000\066\000\028\000\065\000\029\000\064\000\000\000\
\\001\000\017\000\071\000\030\000\070\000\000\000\
\\001\000\017\000\074\000\030\000\070\000\000\000\
\\001\000\017\000\077\000\030\000\070\000\000\000\
\\001\000\018\000\029\000\000\000\
\\001\000\018\000\033\000\034\000\007\000\000\000\
\\001\000\018\000\050\000\000\000\
\\001\000\018\000\056\000\000\000\
\\001\000\026\000\051\000\000\000\
\\001\000\030\000\070\000\031\000\082\000\000\000\
\\001\000\030\000\070\000\031\000\083\000\000\000\
\\001\000\032\000\057\000\000\000\
\\001\000\032\000\062\000\000\000\
\\001\000\032\000\076\000\000\000\
\\001\000\032\000\079\000\000\000\
\\001\000\035\000\000\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\001\000\028\000\000\000\
\\090\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\010\000\023\000\019\000\022\000\020\000\021\000\021\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\091\000\000\000\
\\092\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\010\000\023\000\019\000\022\000\020\000\021\000\021\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\019\000\022\000\020\000\021\000\021\000\020\000\023\000\019\000\
\\024\000\018\000\000\000\
\\099\000\019\000\022\000\020\000\021\000\021\000\020\000\023\000\019\000\
\\024\000\018\000\000\000\
\\100\000\019\000\022\000\020\000\021\000\021\000\020\000\023\000\019\000\
\\024\000\018\000\000\000\
\\101\000\019\000\022\000\020\000\021\000\021\000\020\000\023\000\019\000\
\\024\000\018\000\000\000\
\\102\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\010\000\023\000\019\000\022\000\020\000\021\000\021\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\021\000\020\000\000\000\
\\106\000\021\000\020\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\019\000\022\000\020\000\021\000\021\000\020\000\000\000\
\\110\000\019\000\022\000\020\000\021\000\021\000\020\000\000\000\
\\111\000\000\000\
\\112\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\010\000\023\000\019\000\022\000\020\000\021\000\021\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\010\000\023\000\019\000\022\000\020\000\021\000\021\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\030\000\070\000\000\000\
\\119\000\000\000\
\"
val actionRowNumbers =
"\007\000\031\000\030\000\029\000\
\\027\000\015\000\009\000\016\000\
\\006\000\054\000\006\000\006\000\
\\006\000\035\000\036\000\034\000\
\\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\
\\006\000\006\000\007\000\010\000\
\\017\000\033\000\019\000\008\000\
\\048\000\000\000\003\000\037\000\
\\050\000\049\000\047\000\046\000\
\\045\000\042\000\041\000\040\000\
\\039\000\038\000\028\000\018\000\
\\022\000\006\000\006\000\004\000\
\\044\000\006\000\023\000\011\000\
\\005\000\032\000\053\000\001\000\
\\011\000\012\000\057\000\056\000\
\\011\000\051\000\006\000\013\000\
\\011\000\024\000\014\000\002\000\
\\025\000\058\000\011\000\059\000\
\\043\000\011\000\020\000\021\000\
\\006\000\006\000\052\000\055\000\
\\026\000"
val gotoT =
"\
\\001\000\084\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\030\000\007\000\029\000\000\000\
\\004\000\032\000\000\000\
\\000\000\
\\004\000\033\000\000\000\
\\004\000\034\000\000\000\
\\004\000\035\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\036\000\000\000\
\\004\000\037\000\000\000\
\\004\000\038\000\000\000\
\\004\000\039\000\000\000\
\\004\000\040\000\000\000\
\\004\000\041\000\000\000\
\\004\000\042\000\000\000\
\\004\000\043\000\000\000\
\\004\000\044\000\000\000\
\\004\000\045\000\000\000\
\\002\000\046\000\003\000\003\000\004\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\051\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\056\000\000\000\
\\004\000\057\000\000\000\
\\000\000\
\\000\000\
\\004\000\059\000\000\000\
\\000\000\
\\006\000\061\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\067\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\070\000\000\000\
\\000\000\
\\004\000\071\000\000\000\
\\000\000\
\\006\000\073\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\078\000\000\000\
\\000\000\
\\000\000\
\\006\000\079\000\000\000\
\\000\000\
\\000\000\
\\004\000\082\000\000\000\
\\004\000\083\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 85
val numrules = 33
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID of unit ->  (string) | NUM of unit ->  (int)
 | function of unit ->  (AST.func) | vartype of unit ->  (AST.typ)
 | dec of unit ->  (AST.decl) | formula of unit ->  (AST.exp)
 | statement of unit ->  (AST.state) | program of unit ->  (AST.pro)
 | START of unit ->  (AST.pro)
end
type svalue = MlyValue.svalue
type result = AST.pro
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 34) => true | _ => false
val showTerminal =
fn (T 0) => "TERM"
  | (T 1) => "AND"
  | (T 2) => "OR"
  | (T 3) => "XOR"
  | (T 4) => "EQUALS"
  | (T 5) => "NUM"
  | (T 6) => "FALSE"
  | (T 7) => "TRUE"
  | (T 8) => "NOT"
  | (T 9) => "IMPLIES"
  | (T 10) => "IF"
  | (T 11) => "ELSE"
  | (T 12) => "FI"
  | (T 13) => "EQ"
  | (T 14) => "THEN"
  | (T 15) => "LPAREN"
  | (T 16) => "RPAREN"
  | (T 17) => "ID"
  | (T 18) => "PLUS"
  | (T 19) => "MINUS"
  | (T 20) => "TIMES"
  | (T 21) => "NEGATE"
  | (T 22) => "LESSTHAN"
  | (T 23) => "GREATERTHAN"
  | (T 24) => "LET"
  | (T 25) => "IN"
  | (T 26) => "END"
  | (T 27) => "INT"
  | (T 28) => "BOOL"
  | (T 29) => "ARROW"
  | (T 30) => "FARROW"
  | (T 31) => "COLON"
  | (T 32) => "FN"
  | (T 33) => "FUN"
  | (T 34) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program program1, program1left, 
program1right)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  (program as program1) = program1 ()
 in (program)
end)
 in ( LrTable.NT 0, ( result, program1left, program1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.program program1, _, program1right)) :: _ ::
 ( _, ( MlyValue.statement statement1, statement1left, _)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  (statement
 as statement1) = statement1 ()
 val  (program as program1) = program1 ()
 in (AST.MultiState(statement,program))
end)
 in ( LrTable.NT 1, ( result, statement1left, program1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.program
 (fn _ => let val  (statement as statement1) = statement1 ()
 in (AST.UniState(statement))
end)
 in ( LrTable.NT 1, ( result, statement1left, statement1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.formula formula1, formula1left, 
formula1right)) :: rest671)) => let val  result = MlyValue.statement
 (fn _ => let val  (formula as formula1) = formula1 ()
 in (AST.Exp(formula))
end)
 in ( LrTable.NT 2, ( result, formula1left, formula1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.function function1, function1left, 
function1right)) :: rest671)) => let val  result = MlyValue.statement
 (fn _ => let val  (function as function1) = function1 ()
 in (AST.Function(function))
end)
 in ( LrTable.NT 2, ( result, function1left, function1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _ ::
 ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result
 = MlyValue.dec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (formula as formula1) = formula1 ()
 in (AST.ValDecl(ID,formula))
end)
 in ( LrTable.NT 4, ( result, ID1left, formula1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.function function1, function1left, 
function1right)) :: rest671)) => let val  result = MlyValue.dec (fn _
 => let val  (function as function1) = function1 ()
 in (AST.FuncDecl(function))
end)
 in ( LrTable.NT 4, ( result, function1left, function1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  (NUM as NUM1)
 = NUM1 ()
 in (AST.NumExp(NUM))
end)
 in ( LrTable.NT 3, ( result, NUM1left, NUM1right), rest671)
end
|  ( 8, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.formula (fn _ => (AST.BoolExp(true)))
 in ( LrTable.NT 3, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 9, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val 
 result = MlyValue.formula (fn _ => (AST.BoolExp(false)))
 in ( LrTable.NT 3, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: ( _
, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 in (AST.UniExp(AST.NOT,formula))
end)
 in ( LrTable.NT 3, ( result, NOT1left, formula1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.AND,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.OR,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.XOR,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.EQUALS,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.IMPLIES,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 16, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.formula formula3
, _, _)) :: _ :: ( _, ( MlyValue.formula formula2, _, _)) :: _ :: ( _,
 ( MlyValue.formula formula1, _, _)) :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => let val  
formula1 = formula1 ()
 val  formula2 = formula2 ()
 val  formula3 = formula3 ()
 in (AST.IfExp(formula1,formula2,formula3))
end)
 in ( LrTable.NT 3, ( result, IF1left, FI1right), rest671)
end
|  ( 17, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.formula (fn _ => let val  (formula as formula1
) = formula1 ()
 in (AST.ParExp(formula))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.PLUS,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.MINUS,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.TIMES,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 21, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: ( _
, ( _, NEGATE1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 in (AST.UniExp(AST.NEGATE,formula))
end)
 in ( LrTable.NT 3, ( result, NEGATE1left, formula1right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.LESSTHAN,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.GREATERTHAN,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 24, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: _ :: ( _, ( MlyValue.dec dec1, _, _)) :: ( _, ( _,
 LET1left, _)) :: rest671)) => let val  result = MlyValue.formula (fn
 _ => let val  (dec as dec1) = dec1 ()
 val  (formula as formula1) = formula1 ()
 in (AST.LetExp(dec,formula))
end)
 in ( LrTable.NT 3, ( result, LET1left, END1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _
 :: ( _, ( MlyValue.vartype vartype2, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.vartype vartype1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _
)) :: _ :: ( _, ( _, FN1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (ID as ID1) = ID1 ()
 val  vartype1 = vartype1 ()
 val  vartype2 = vartype2 ()
 val  (formula as formula1) = formula1 ()
 in (AST.Fn(ID,vartype1,vartype2,formula))
end)
 in ( LrTable.NT 3, ( result, FN1left, formula1right), rest671)
end
|  ( 26, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.formula 
formula2, _, _)) :: ( _, ( MlyValue.formula formula1, _, _)) :: ( _, (
 _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  formula1 = formula1 ()
 val  formula2 = formula2 ()
 in (AST.FunExp(formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.formula (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (AST.VarExp(ID))
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _
 :: ( _, ( MlyValue.vartype vartype2, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.vartype vartype1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _
)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _))
 :: rest671)) => let val  result = MlyValue.function (fn _ => let val 
 ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  vartype1 = vartype1 ()
 val  vartype2 = vartype2 ()
 val  (formula as formula1) = formula1 ()
 in (AST.Fun(ID1,ID2,vartype1,vartype2,formula))
end)
 in ( LrTable.NT 6, ( result, FUN1left, formula1right), rest671)
end
|  ( 29, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.vartype (fn _ => (AST.Type(AST.INT)))
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 30, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.vartype (fn _ => (AST.Type(AST.BOOL)))
 in ( LrTable.NT 5, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.vartype vartype2, _, vartype2right)) :: _
 :: ( _, ( MlyValue.vartype vartype1, vartype1left, _)) :: rest671))
 => let val  result = MlyValue.vartype (fn _ => let val  vartype1 = 
vartype1 ()
 val  vartype2 = vartype2 ()
 in (AST.CurrType(vartype1,vartype2))
end)
 in ( LrTable.NT 5, ( result, vartype1left, vartype2right), rest671)

end
|  ( 32, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.vartype 
vartype1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.vartype (fn _ => let val  (vartype as vartype1
) = vartype1 ()
 in (AST.ParType(vartype))
end)
 in ( LrTable.NT 5, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Algebra_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun FARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
end
end
