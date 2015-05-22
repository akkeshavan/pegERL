%% @author SONY
%% @doc @todo Add description to calcast.


-module(calcast).
-import(peg,[empty/0,
			 matcher/1,matcher/2,
			 oneOf/2,oneOf/3,
			 seq/1,seq/2, 
			 rep/1,rep/2,
			 entryPoint/1,
			 forward_ref/1,
			 register_forward/2
			 ]
	   ).
%% ====================================================================
%% API functions
%% ====================================================================
-export([buildAst/1,evalAst/1,polish/1,test/0]).

grammar()->
	
	Num=matcher("^[0-9]+",fun (V)->{ok,leaf(num,V)} end),
	Op=matcher("^[\+|\-]",fun(O)-> {ok,operator(O)} end),
	MultOp=matcher("^[\*|\/]",fun(O)-> {ok,operator(O)} end),
	Lpar=matcher("^\\("),
	Rpar=matcher("^\\)"),
	Nested=seq([Lpar,forward_ref(expr),Rpar], fun ([_,E,_])->{ok,leaf(nested,E)} end ),
    Factor=oneOf(Num,Nested),
    Term=seq([Factor,rep(seq([MultOp,Factor]))],fun ([F,MFList])->{ok,exprTree(F,MFList)}end),
    Expr=seq([Term,rep(seq([Op,Term]))],fun ([T,OTList])->{ok,exprTree(T,OTList)}end),
    Final=seq([Expr,empty()],fun ([E,_])->{ok,leaf(expr,E)} end),
    register_forward(expr,Expr),
 
    entryPoint(Final).

buildAst(Str)-> Parser=grammar(), Parser(Str).
parse(Str)->evalAst(buildAst(Str)).

evalAst(Expr)->
	case Expr of
		{error, ErrorStr}->ErrorStr;
		{ok,Ast}-> evalAst(Ast);
		{num,Value}->{V,_}=string:to_integer(Value),V;
		{nested,Value}-> evalAst(Value);
		{expr,Value}-> evalAst(Value);
        {plus,Left,Right}->evalAst(Left)+evalAst(Right);
		{minus,Left,Right}->evalAst(Left)-evalAst(Right);
		{mult,Left,Right}->evalAst(Left)*evalAst(Right);
		{divop,Left,Right}->evalAst(Left) div evalAst(Right)
	end.

polish(Expr)->
	case Expr of
		{error, ErrorStr}->ErrorStr;
		{ok,Ast}-> polish(Ast);
		{num,Value}->Value;
		{expr,Value}->polish(Value);
		{nested,Value}-> "("++polish(Value)++ ")";
        {plus,Left,Right}->"+"++polish(Left)++" "++polish(Right);
		{minus,Left,Right}->"-"++polish(Left)++" "++polish(Right);
		{mult,Left,Right}->"*"++polish(Left)++" "++polish(Right);
		{divop,Left,Right}->"/"++polish(Left)++" "++polish(Right)
	end.

test()->Tests=["100","100+200",
		              "100+3*2","4*5",
		              "100+(30/6)*2",
		              "(100*2)",
		              "(((100*2)/4)+5)/5",
		                "(100(30/6)*2",
		              "100abc" ],
	lists:foreach(fun(E)->print_result(E,parse(E),polish(buildAst(E))) end, Tests).
	 
	

%% ====================================================================
%% Internal functions
%% ====================================================================

operator(Op)->
	case Op of
		"+"-> plus;
		"-"->minus;
		"*"->mult;
		"/"->divop
	end.

exprTree(Num,OpTermList)->
	case OpTermList of
		[]->Num;
		_-> lists:foldl(fun (Elem,Left)-> [Op,Right]=Elem,tree(Op,Left,Right) end, Num, OpTermList)
    end.


tree(Type,Left,Right)->{Type,Left,Right}.
leaf(Type, Value)-> {Type,Value}.

print_result(Exp,Value,Pol)-> io:format("\r\nExp:~p Value=> ~p\r\n polish=>~p",[Exp,Value,Pol]).