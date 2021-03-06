-module(kvx).
-behaviour(application).
-behaviour(supervisor).
-description('KVX Abstract Chain Store').
-include_lib("stdlib/include/assert.hrl").
-include("api.hrl").
-include("metainfo.hrl").
-include("stream.hrl").
-include("cursors.hrl").
-include("kvx.hrl").
-include("backend.hrl").
-export([dump/0,check/0,metainfo/0,ensure/1,seq_gen/0,fold/6,fold/7]).
-export(?API).
-export(?STREAM).
-export([init/1, start/2, stop/1]).

init([]) -> {ok, { {one_for_one, 5, 10}, []} }.
start(_,_) -> supervisor:start_link({local, kvx}, kvx, []).
stop(_) -> ok.

% kvx api

dba()              -> application:get_env(kvx,dba,store_mnesia).
kvx_stream()       -> application:get_env(kvx,dba_st,kvx_stream).
all(Table)         -> all     (Table, #kvx{mod=dba()}).
delete(Table,Key)  -> delete  (Table, Key, #kvx{mod=dba()}).
get(Table,Key)     -> ?MODULE:get     (Table, Key, #kvx{mod=dba()}).
index(Table,K,V)   -> index   (Table, K,V, #kvx{mod=dba()}).
%change_storage(Table,Type) -> change_storage(Table,Type, #kvx{mod=dba()}).
join()             -> join    ([],    #kvx{mod=dba()}).
dump()             -> dump    (#kvx{mod=dba()}).
join(Node)         -> join    (Node,  #kvx{mod=dba()}).
leave()            -> leave   (#kvx{mod=dba()}).
count(Table)       -> count   (Table, #kvx{mod=dba()}).
put(Record)        -> ?MODULE:put     (Record, #kvx{mod=dba()}).
fold(Fun,Acc,T,S,C,D) -> fold (Fun,Acc,T,S,C,D, #kvx{mod=dba()}).
stop()             -> stop_kvx(#kvx{mod=dba()}).
start()            -> start   (#kvx{mod=dba()}).
%destroy()          -> destroy (#kvx{mod=dba()}).
ver()              -> ver(#kvx{mod=dba()}).
dir()              -> dir     (#kvx{mod=dba()}).
seq(Table,DX)      -> seq     (Table, DX, #kvx{mod=dba()}).

% stream api

top  (X) -> (kvx_stream()):top (X).
bot  (X) -> (kvx_stream()):bot (X).
next (X) -> (kvx_stream()):next(X).
prev (X) -> (kvx_stream()):prev(X).
drop (X) -> (kvx_stream()):drop(X).
take (X) -> (kvx_stream()):take(X).
save (X) -> (kvx_stream()):save(X).
cut  (X,Y) -> (kvx_stream()):cut (X,Y).
add  (X) -> (kvx_stream()):add (X).
append  (X, Y) -> (kvx_stream()):append (X, Y).
load_reader (X) -> (kvx_stream()):load_reader(X).
writer      (X) -> (kvx_stream()):writer(X).
reader      (X) -> (kvx_stream()):reader(X).
ensure(#writer{id=Id}) ->
   case kvx:get(writer,Id) of
        {error,_} -> kvx:save(kvx:writer(Id)), ok;
        {ok,_}    -> ok end.

metainfo() ->  #schema { name = kvx, tables = core() }.
core()    -> [ #table { name = id_seq, fields = record_info(fields,id_seq), keys=[thing]} ].

initialize(Backend, Module) ->
    [ begin
        Backend:create_table(T#table.name, [{attributes,T#table.fields},
               {T#table.copy_type, [node()]},{type,T#table.type}]),
        [ Backend:add_table_index(T#table.name, Key) || Key <- T#table.keys ],
        T
    end || T <- (Module:metainfo())#schema.tables ].

all(Tab,#kvx{mod=DBA}) -> DBA:all(Tab).
start(#kvx{mod=DBA}) -> DBA:start().
stop_kvx(#kvx{mod=DBA}) -> DBA:stop().
%change_storage(Type) -> [ change_storage(Name,Type) || #table{name=Name} <- kvx:tables() ].
%change_storage(Table,Type,#kvx{mod=DBA}) -> DBA:change_storage(Table,Type).
%destroy(#kvx{mod=DBA}) -> DBA:destroy().
join(Node,#kvx{mod=DBA}) -> DBA:join(Node).
leave(#kvx{mod=DBA}) -> DBA:leave().
ver(#kvx{mod=DBA}) -> DBA:version().
tables() -> lists:flatten([ (M:metainfo())#schema.tables || M <- modules() ]).
table(Name) when is_atom(Name) -> lists:keyfind(Name,#table.name,tables());
table(_) -> false.
dir(#kvx{mod=DBA}) -> DBA:dir().
modules() -> application:get_env(kvx,schema,[]).
cursors() ->
    lists:flatten([ [ {T#table.name,T#table.fields}
        || #table{name=Name}=T <- (M:metainfo())#schema.tables, Name == reader orelse Name == writer  ]
    || M <- modules() ]).

fold(___,Acc,_,[],_,_,_) -> Acc;
fold(___,Acc,_,undefined,_,_,_) -> Acc;
fold(___,Acc,_,_,0,_,_) -> Acc;
fold(Fun,Acc,Table,Start,Count,Direction,Driver) ->
    try
    case kvx:get(Table, Start, Driver) of
         {ok, R} -> Prev = element(Direction, R),
                    Count1 = case Count of C when is_integer(C) -> C - 1; _-> Count end,
                    fold(Fun, Fun(R,Acc), Table, Prev, Count1, Direction, Driver);
          _Error -> Acc
    end catch _:_ -> Acc end.

seq_gen() ->
    Init = fun(Key) ->
           case kvx:get(id_seq, Key) of
                {error, _} -> {Key,kvx:put(#id_seq{thing = Key, id = 0})};
                {ok, _} -> {Key,skip} end end,
    [ Init(atom_to_list(Name))  || {Name,_Fields} <- cursors() ].


put(Records,#kvx{mod=Mod}) when is_list(Records) -> Mod:put(Records);
put(Record,#kvx{mod=Mod}) -> Mod:put(Record).
get(RecordName, Key, #kvx{mod=Mod}) -> Mod:get(RecordName, Key).
delete(Tab, Key, #kvx{mod=Mod}) -> Mod:delete(Tab, Key).
count(Tab,#kvx{mod=DBA}) -> DBA:count(Tab).
index(Tab, Key, Value,#kvx{mod=DBA}) -> DBA:index(Tab, Key, Value).
seq(Tab, Incr,#kvx{mod=DBA}) -> DBA:seq(Tab, Incr).
dump(#kvx{mod=Mod}) -> Mod:dump().

% tests

-record(emails, { id    = [] :: [] | integer(),
                  next  = [] :: [] | integer(),
                  prev  = [] :: [] | integer(),
                  email = [] :: [] | binary() }).

check() ->
    Id  = {list,kvx:seq(writer,1)},
    X   = 5,
    _W   = kvx:save(kvx:writer(Id)),
    #reader{id=R1} = kvx:save(kvx:reader(Id)),
    #reader{id=R2} = kvx:save(kvx:reader(Id)),
    [ kvx:save(kvx:add((kvx:writer(Id))#writer{args=#emails{}})) || _ <- lists:seq(1,X) ],
    Bot = kvx:bot(kvx:load_reader(R1)),
    Top = kvx:top(kvx:load_reader(R2)),
    #reader{args=F} = kvx:take(Bot#reader{args=20,dir=0}),
    #reader{args=B} = kvx:take(Top#reader{args=20,dir=1}),
    ?assertMatch(X,length(F)),
    ?assertMatch(F,lists:reverse(B)).
