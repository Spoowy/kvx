-module(kvx_st).
-description('KVX STREAM NATIVE ROCKS').
-include("kvx.hrl").
-include("stream.hrl").
-include("metainfo.hrl").
-export(?STREAM).

ref() -> kvx_rocks:ref().

% section: kvx_stream prelude

se(X,Y,Z)  -> setelement(X,Y,Z).
set(X,Y,Z) -> setelement(X,Z,Y).
e(X,Y)  -> element(X,Y).
c0(R,V) -> se(1, R, V).
c1(R,V) -> se(#reader.id,    R, V).
c2(R,V) -> se(#reader.pos,   R, V).
c3(R,V) -> se(#reader.cache, R, V).
c4(R,V) -> se(#reader.args,  R, V).
c5(R,V) -> se(#reader.feed,  R, V).
c6(R,V) -> se(#reader.dir,   R, V).
wf(R,V) -> se(#writer.first, R, V).
si(M,T) -> se(#it.id, M, T).
el(X,T) -> e(X, T).
tab(T)  -> e(1, T).
id(T)   -> e(#it.id, T).
pos(T)  -> e(#reader.pos, T).
args(T) -> e(#writer.args, T).
dir(0)  -> top;
dir(1)  -> bot.
acc(0)  -> next;
acc(1)  -> prev.

% section: next, prev

top  (#reader{}=C) -> C.
bot  (#reader{}=C) -> C.
next (#reader{}=C) -> C.
prev (#reader{}=C) -> C.
up   (#reader{}=C) -> C#reader{dir=0}.
down (#reader{}=C) -> C#reader{dir=1}.

% section: take, drop

drop(#reader{args=N}) when N < 0 -> #reader{};

drop(#reader{args=N,feed=Feed,cache=I}=C) when N == 0 ->
   Key = list_to_binary(lists:concat(["/",io_lib:format("~p",[Feed])])),
   First = case rocksdb:iterator_move(I, {seek,Key}) of
                {ok,_,Bin} -> binary_to_term(Bin,[safe]);
                       _ -> [] end,
   C#reader{cache=First};

drop(#reader{args=N,feed=Feed,cache=I}=C) when N > 0 ->
   Key   = list_to_binary(lists:concat(["/",io_lib:format("~p",[Feed])])),
   First = rocksdb:iterator_move(I, {seek,Key}),
   Term  = lists:foldl(
    fun (_,{{ok,K,_},{_,X}}) when N > X -> {K,{<<131,106>>,N}};
        (_,{{ok,K,Bin},{A,X}}) when N =< X->
           case binary:part(K,0,size(Key)) of
                Key -> {rocksdb:iterator_move(I,next),{Bin,X+1}};
                  _ -> {{error,range},{A,X}} end;
        (_,{_,{_,_}}) -> {[],{<<131,106>>,N}}
     end,
           {First,{<<131,106>>,1}},
           lists:seq(0,N)),
   C#reader{cache=binary_to_term(element(1,element(2,Term)))}.

take(#reader{args=N,feed=Feed,cache=I,dir=Dir}=C) ->
   Key   = list_to_binary(lists:concat(["/",io_lib:format("~p",[Feed])])),
   First = rocksdb:iterator_move(I, {seek,Key}),
   Res   = kvx_rocks:next(I,Key,size(Key),First,[],[],N,0),
   C#reader{args= case Dir of 0 -> Res; 1 -> lists:reverse(Res); _ -> [] end}.

% new, save, load, up, down, top, bot

load_writer (Id) -> case kvx:get(writer,Id) of {ok,C} -> C; E -> E end.
load_reader (Id) ->
    case kvx:get(reader,Id) of
         {ok,#reader{}=C} -> C#reader{cache=element(2,rocksdb:iterator(ref(),[]))};
              E -> E end.

writer (Id) -> case kvx:get(writer,Id) of {ok,W} -> W; {error,_} -> #writer{id=Id} end.
reader (Id) ->
    case kvx:get(writer,Id) of
         {ok,#writer{}} ->
             {ok,I} = rocksdb:iterator(ref(), []),
             #reader{id=kvx:seq([],[]),feed=Id,cache=I};
         {error,X} -> {error,X} end.
save (C) -> NC = c4(C,[]), N2 = c3(NC,[]), kvx:put(N2), N2.


% add

add(#writer{args=M}=C) when element(2,M) == [] -> add(si(M,kvx:seq([],[])),C);
add(#writer{args=M}=C) -> add(M,C).

add(M,#writer{id=Feed,count=S}=C) -> NS=S+1,
    rocksdb:put(ref(),
       <<(list_to_binary(lists:concat(["/",io_lib:format("~p",[Feed]),"/"])))/binary,
         (term_to_binary(id(M)))/binary>>, term_to_binary(M), [{sync,true}]),
    C#writer{cache=M,count=NS}.
