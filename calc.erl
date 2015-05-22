%% @author SONY
%% @doc @todo Add description to calc.


-module(calc).
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
-export([parse/1,test/0]).

grammar()->
	
	Num=matcher("^[0-9]+",fun (V)->{R,_}=string:to_integer(V),{ok,R} end),
	Op=matcher("^[\+|\-]"),
	MultOp=matcher("^[\*|\/]"),
	Lpar=matcher("^\\("),
	Rpar=matcher("^\\)"),
	Nested=seq([Lpar,forward_ref(expr),Rpar], fun ([_,E,_])->{ok,E} end ),
    Factor=oneOf(Num,Nested),
    Term=seq([Factor,rep(seq([MultOp,Factor]))],fun ([F,MFList])->{ok,compute(F,MFList)} end),
    Expr=seq([Term,rep(seq([Op,Term]))],fun ([T,OTList])->{ok,compute(T,OTList)} end),
    Final=seq([Expr,empty()],fun ([E,_])->{ok,E} end),
    register_forward(expr,Expr),
 
    entryPoint(Final).
    
parse(Str)-> Parser= grammar(), Parser(Str).	
test ()->
	Tests=["100","100+200",
		              "100+3*2","4*5",
		              "100+(30/6)*2",
		              "(100*2)",
		              "(((100*2)/4)+5)/5",
		                "(100(30/6)*2",
		              "100abc" ],
	lists:foreach(fun(E)->print_result(E,calc:parse(E)) end, Tests).
%% ====================================================================
%% Internal functions
%% ====================================================================

compute(Num,OpNumList)->
	case OpNumList of
		[]->Num;
		_-> lists:foldl(fun (OpNum,Acc)->evaluate([Acc,OpNum]) end, Num, OpNumList)
    end.


evaluate(Elem)->
	[E1,[OP,E2]]=Elem,
	case OP of
      "+"-> E1+E2;
      "-"-> E1-E2;
      "*"-> E1*E2;
      "/"->E1 div E2
    end.

 

print_result(Exp,Res)-> io:format("\nExp:~p Result=> ~p\r\n",[Exp,Res]).
 
 