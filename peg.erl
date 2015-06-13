%% @author SONY
%% @doc @todo Add description to peg.

-module(peg).

 
 
%% ====================================================================
%% API functions
%% ====================================================================
-export(
   		[
		 empty/0,
		 empty/1,
		 matcher/1,
		 matcher/2,
		 oneOf/2,
		 oneOf/3,
		 seq/1,
		 seq/2,
		 rep/1,
		 rep/2,
		 opt/1,
		 opt/2,
		 entryPoint/1,
		 forward_ref/1,
		 register_forward/2
		]
	   ).
	     

empty()-> fun(Str)->case remove_leading(Str)=="" of
						true-> token(empty,leading_spcs(Str),"",null);
					    false->token(error,leading_spcs(Str),Str,"")
					end
		  end.


empty(MapFun)->map(empty(),MapFun).

matcher(Reg)-> fun(Str)->
					  TrimStr=remove_leading(Str),
					   case re:run(TrimStr,Reg) of
						    {match,[{From,To}]}->token(unit,leading_spcs(Str),subStr(TrimStr,From,To),
													   null);
					        nomatch->token(error,leading_spcs(Str),Str,"")
				         end
				 end.       
		   
matcher(Reg,MapFun)->map(matcher(Reg),MapFun).

oneOf(One,TheOther)->
	fun(Str)->
			Result=One(Str),
			case tokType(Result) of
				error->TheOther(Str);
				_->Result
			end
	end.

oneOf(One,TheOther,MapFun)->map(oneOf(One,TheOther),MapFun).

seq(ParserList)->
	Sequence=lists:foldl(fun (E,Prev)->bind(Prev,E) end, null, ParserList),
    fun(Str)->
		Result=Sequence(Str),
        case Result of
			{error,_,_,_}-> Result;
            {_,WS,Element,Value}->{seq,WS,Element,Value}
		end
        
	end.

forward_ref(Key)->
	fun (Str)->
		 Parser=get(Key),
		 Parser(Str)
    end.
seq(ParserList,MapFun)->map(seq(ParserList),MapFun).

rep(Parser)->fun (Str)->rep_iter(Parser,Str,[]) end.
rep(Parser,MapFun)->map(rep(Parser),MapFun).

opt(Parser)->
	fun (Str)->
		R=Parser(Str),
		case R of
			error-> token(rep,"",[],null);
			{_,WS,Element,Value}->token(rep,WS,[Element],Value)
		end
	end.
opt(Parser,Mapfn)->map(opt(Parser),Mapfn).

register_forward(Key,Parser)-> put(Key,Parser).
			 
entryPoint(Parser)->
	 fun (Str)->
			  Result=Parser(Str),
			  case tokType(Result) of
					error->{error,subStr(Str,0,string:len(Str)-string:len(tokElement(Result)))
			                       ++"^^"++tokElement(Result)};
                    mapped->{ok,tokValue(Result)};
                    _  ->{ok,tokElement(Result)}
			  end
     end.
 
%% ====================================================================
%% Internal functions
%% ====================================================================

leading_spcs(Str)->
     case re:run(Str,"^[\s\t\n\r]+") of
        {match,[{From,To}]}->subStr(Str,From,To);
        nomatch ->""
     end.
remove_leading(Str)->
	case re:run(Str,"^[\s|\t|\n|\r]+") of
        {match,[{_,To}]}->slice(Str,To);
        nomatch ->Str
     end.



%% 0-based versions of slice


slice(Str,At)->string:sub_string(Str,At+1).

%% 0-based version of sub_string 
subStr(Str,From,To)->string:sub_string(Str,From+1,To).

 
map(Parser,MapFun)->
	fun(Str)->
			Result=Parser(Str),
			case tokType(Result) of
				error->Result;
			    _-> MappedResult=MapFun(tokenToArgs(Result)),
					case MappedResult of
					 {ok,Value}->token(mapped,"",Result,Value);
					 {error,EStr}->token(error,"",Str,EStr)
					end
			end
	end.
 

bind(Parser1,Parser2)->
	case Parser1 of
		null->Parser2;

		_->fun(Str)->
			Result1=Parser1(Str),
			case Result1 of
				{error,_,_,_}->Result1;
                 _ -> Result2=Parser2(advance(Str,Result1)),
                  	  case Result2 of
						{error,_,_,_ }->Result2;
                  	   _->flatten(Result1,Result2)
					  end
			end
     end
 end.

 rep_iter(Parser,Str,Acc)->
		R=Parser(Str),
		case tokType(R) of
			error-> token(rep,"",Acc,null);
			 _-> rep_iter(Parser,advance(Str,R),lists:append(Acc, [R]))
		end.
	
flatten(R1,R2)->
	case R1 of 
		{bind,_,Element,_}-> token(bind,"",lists:append(Element,[R2]),null);
		_ ->token(bind,"",[R1,R2],null)
    end.


advance(PrevStr,Result)->
	case tokType(Result) of 
		error->PrevStr;
        _  -> slice(PrevStr,tokenLen(Result))
		end.

%% Token functions

token(Type,Ws,Element,Value)->{Type,Ws,Element,Value}.
 
tokType({Type,_,_,_})->Type.
tokWS({_,WS,_,_})->WS.
tokElement({_,_,Element,_})->Element.
tokValue({_,_,_,Value})->Value.

tokenLen(Tok)->
	case tokType(Tok) of
       unit->string:len(tokElement(Tok))+string:len(tokWS(Tok));
	   error->0;
	   empty ->string:len(tokWS(Tok));
	   bind->lists:foldl(fun(Elem,Prev)->Prev+tokenLen(Elem) end,0,tokElement(Tok));
       seq->lists:foldl(fun(Elem,Prev)->Prev+tokenLen(Elem) end,0,tokElement(Tok));
       rep ->lists:foldl(fun(Elem,Prev)->Prev+tokenLen(Elem) end,0,tokElement(Tok));
       mapped->tokenLen(tokElement(Tok))
    end.

tokenToArgs(Tok)->
	
	case tokType(Tok )of
	   unit->tokElement(Tok);
	   empty->[""];
	   
       seq-> lists:map(fun(Elem)->tokenToArgs(Elem) end,tokElement(Tok));
       rep->lists:map(fun(Elem)->
						tokenToArgs(Elem)
					   end,tokElement(Tok));
       mapped->  tokValue(Tok)
		       
    end.