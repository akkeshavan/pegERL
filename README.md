# Erlang Parser Combinator Library for Parser Expression Grammars (PEG) 

 This parser combinator library is based on the example developed in my book 'Deep Dive: The Magical world
 of functional programming'. The original example was developed in functional Javascript. 
 
 
 
 The parser combinator library consists of just one file peg.erl and exports the following functions
 
 ```erlang
 empty/0,empty/1,
 matcher/1,matcher/2,
 oneOf/2,oneOf/3,
 seq/1,seq/2,
 rep/1,rep/2,
 opt/1.opt/2,
 entryPoint/1,
 forward_ref/1,
 register_forward/2
 ``` 
  
Two examples have been developed using the peg library
  
- calc.erl-> directly evaluates an arithmetic expression
- calcast.erl-> creates an abstract syntax tree for the grammar and has two functions
                 that evaluate the AST --  ```evaluate(Ast)``` -> computes the value of the expressions
                 and ```polish(Ast)``` returns a "polish" version of  the input.
                 
Parsers can be created for any language that can be expressed as a parser expression grammar.
   
Both files export a simple test/0 function that can be used to test the code.
   
## Usage  

The file peg.erl exports the following functions.  
 
All the parser functions returned by these functions ( except entryPoint()) return a "token" data type. 
Tokens can be of type "error", "unit", 'seq','rep' 
Token functions defined in pegtoken.js are straight forward.   The tokens returned by the parsers 
are need not be used ( as the examples in calc.erl and calcast.erl show).
 
In the documentation, token descriptions have been given, in case anyone wants to use the tokens directly, instead of using
the MapFun. ( read on it will be clear).

### 1. empty/1, empty/2
 
#### 1.1 empty()
 
 Returns a parser that accepts a string is empty or not. The function empty() skips the leading
 whites paces and returns a token whihc is a 4-tuple {Type=atom, WS=white spaces, Element=<matched string,
 Value=null}
  
  Example: 
```erlang 
   EOF=empty(), 
   EOF("")=> {empty,WS="",Element="",Value=null},
   EOF(" \t")=> {empty,WS="",Element="",Value=null}, 
   EOF("abc")=> {error,WS="", "abc", null).
```   

#### 1.2 empty(MapFn)
   
where Mapfn= fun ("")=> {ok, <mappedValue>} or {error,<errorString>).
 
Returns: fun (Str)=> {mapped,"",{empty,....},mappedValue}
                        {error, "", Str, <Error returned by mapfun ifn any>}
                          
a function that applies MapFun to  the result of applying  empty() on a string .
   
The mapFun is rarely used in case of empty() as there is no real need to transform the result.
   
The final result will be of  atoke of the the following structure:
   
if the mapFn returns {ok, Somevalue}
  
{mapped,"", {empty,"","",null}, SomeValue}

Otherwise if it returns {error, ErroStr]

{error,"",Str,ErrorStr}. ( Str is the one that is passed to the function.)
   
   
### 2. matcher/1, matcher/2
 
#### 2.1 matcher(RegEx)

Returns a function that accepts a string and returns whether the string begins with the pattern defined by
regEx. regEx must begin with a '^';  The function returned by matcher() skips leading whitespaces.
 
 Example:
```erlang 
 Num=matcher("^[0-9]+"),
 Num("100")=> {unit,"","100", null}.
 Num("\t\n100")=> {unit,"\t\n","100",null}.
 Num("abc")=> {error,"","abc",null}
```

#### 2.2 matcher(RegEx, MapFn)
    Where MapFn = fun (Str)->  {ok, <mappedValue>} or {error,<errorString>).
    
    Example,
```erlang    
 Num=matcher("^[0-9]+",fun (V)->{R,_}=string:to_integer(V),{ok,R} end),
 
 Num("100")=> {mapped,"",{unit,"","100",null},100(integer 100)}
 Num("abc")=> {error, "","100",null}
 
 TelNo=matcher("^[0-9]+",fun (V)-> case string:len(V) of
 								    10 -> {ok,V};
 								    _  -> {error, "tel number must be 10 characters"})
 								    
TelNo("1234567890")=> {mapped,"",{unit,"","1234567890",null},"1234567890"}
TelNo("123")=> {error, "tel no must be 10 characters"}
```
                               
    
### 3. rep/1, rep/2

#### 3.1 rep(Parser)

This is repeated application of the same parser on a string after advancing the input string by 
the previous match.

The function returns an array in which each element represents a successful application of the the parser. 

If no matches are found
it returns an token whose third element is an empty array.

Rep returns a parser that recognizes a sequence of patterns recognized by Parser. 
if P= Fun(Str)-> Ta  then the function returne by rep() is
  fun (Str)=> {rep,"", [Ta1,Ta2,Ta2.. Tn].
  
 The parser created using rep() never returns an error-- it returns an empty list of no matches
 are found.
 
  
Example:
```erlang
Letter= matcher("^[a-z]")
 Digit=matcher("^[0-9]")
 Letters=rep(Letter).
 Digits=rep(Digit).
 
 Letters("abc")=> {rep,"",[{unit,"", "a",null},{unit,"", "b",null},{unit,"","c",null}],null}
```
( The third element of the tuple is alwasy the raw result of applying a
parser. the fourth element has a value only if it is of type mapped).

#### 3.2 rep/2
  
  rep(Parser, MapFun)
  
  where MapFun = fun ([list of tokens]) -> usual map return values.
  
  Note, that teh [list of tokens] can be an empty list. so any Mapfn that you write for
  reps should address both the cases:
  
```erlang
SomeParser=rep(Parser, fun(Tokens)-> case Tokens of 
                                         []->
                                         _-> end)
```

### 4. seq/1, seq/2

#### 4.1 seq([Parser1, Parser2,... ParserN)

Returns a parser that determines whether a string matches that sequence of parsers p1...pn. The parser
function returned by  seq() returns an error if any of the parsers  ( [p1,p1..pn]) returns an error. Otherwise
it returns an array of values where each element corresponds to the return values of p1,p2..pn respectively.
  
if p1 returns r1, p1 returns r2, .... pn returns rn then the function returned by seq() returns
[r1,r2....rn] on success and a token or the type "error" otherwise.
  
if S=seq([P1->R1,P2->R2,...Pn) 
then S= fun(Str)-> {seq,"", [R1,R2,...Rn],null}
  
The parsers can be any of the functions created using the peg functions.
  
  Example:
```erlang  
  Id=seq(Letter,Rep(LetterOrDigit))  // see Letter and LetterOr Digit above
  
  Id("A123")=> {seq,"", [{unit,"","A",null},{rep,"",[{unit,"","1",null},
                                                     {unit,"","2",null},
                                                     {unit,"","3",null}]],null).
```

#### 4.1 seq([P1,P2,..Pn],MapFun) 

Returns a parser  whose result is the application of the MapFun on result of the parser returner by seq/1.

MapFun= Fun([R1,R2,...Rn])-> {ok,Value} or {error, ErrorStr}.

where R1 is the result of P1 , R2 is the result of P2 etc.

The MapFn will recieve a list as ist argumenr -- each one of the elements of the list will correspond to the following:

-if parser P[i] is created by matcher/1 it will get the element R[i] will be a string
-If the parser P[i] is created rep() or seq() it the element Ri will be a list
-if P[i] has been created by matcher/2 or seq/2 or rep/2, then R[i] will be the Value teturend by the 
corresponding MapFun.

This may seem confusing, but please study the two examples, calc.erl, calcast.erl.

### 5. oneOf/2, oneOf/3

#### 5.1 oneOf(Parser1,Parse2) 
 Returns  a parser function that accepts a string. The parser returned by oneOf() returns a valid token
 if one of the parsers ( Parser1 or Parser 2) matches the string. Parsers 1 and 2 can be any one of the
 peg functions matcher,seq,oneOf or rep 
 
 Example:
```erlang
 Letter= matcher("^[a-z]")
 Digit=matcher("^[0-9]")
 LetterOrDigit=oneOf(Letter,Digit)
 
 LetterOrDigit("a")-> {unit,"","a",null).
 LetterOrDigit("1")-> {unit,"","1",null).
 LetterOrDigit("+")-> {error,"","+",null).
```

#### oneOf(Parser1,Parser2, MapFun)

MapFun similar to other map functions.

### 6 entryPoint/1

Returns a function that acts as the entrypoint or the final parser in the PEG definition.

#### entryPoint(Parser)
  
  Returns a function:
  
  fun(Str)->  {ok, result of Parser(Str)) or {error, ErrorString}
  
### register_forward(term,Parser) and forward_ref(term).

register_forward(term,Parser) saves the {term,Parser} in the process dictionary.
forward_ref(term) retrives the parser

PEG grammars are recursive by nature. For example, our calculator grammar

```
Num : number
nested: '(' expr ')
factor: num OR nested
term: factor {'*',factor}
expr: term {'+', term}
```

If we map this grammar using our peg library

```erlang
Num = matcher(...)
Nested = seq ( matcher("^\\(", Expr,matcher("^\\)"))
Factor= oneOf(Num,Nested).
Term= seq(Factor,rep(seq(..))
Expr= seq (...._)
```

At the time when Nested is defined -- Expr will be an unbound variable. The definition of Nested 
has a forward reference to the Expr.

This can be fixed as follows:

```erlang
Num = matcher(...)
Nested = seq ( matcher("^\\(", forward_ref(expr) ,matcher("^\\)")) <- forward refernce
Factor= oneOf(Num,Nested).
Term= seq(Factor,rep(seq(..))
Expr= seq (...._)
register_forward(expr,Expr)  <- right after expr has been define
```

 This will fix the problem. I can hear purist complaining about using the process dict. 
 What the hell!! as long as one is using it to initialize values ( not as a mutable data storage)
 it is fine.
 
  
Please study calc.erl, calcAST.erl to understand the usage.

#### 7 opt/1, opt2

Work exactly like rep() except that rep() matches zero or more occurrences whereas opt()
matches zero or one occurrence of the construct recognized by the parser. Opt() returns 
an empty list {} for no match and an list with one result in case of a match [Result]. See
rep() above.

## Developing



### Tools
 
 Created using Eclipse   
