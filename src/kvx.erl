-module(kvx).
-description('KVX Abstract Chain Storage').
-include_lib("stdlib/include/assert.hrl").
-include("api.hrl").
-include("metainfo.hrl").
-include("stream.hrl").
-include("kvx.hrl").
-export([info/3,warning/3,error/3,trace/3,dump/0,check/0]).
-export(?API).
-export(?STREAM).
-export([metainfo/0]).

% kvx api

dba()              -> application:get_env(kvx,dba,store_mnesia).
all(Table)         -> all     (Table, #kvx{mod=dba()}).
delete(Table,Key)  -> delete  (Table, Key, #kvx{mod=dba()}).
get(Table,Key)     -> ?MODULE:get     (Table, Key, #kvx{mod=dba()}).
index(Table,K,V)   -> index   (Table, K,V, #kvx{mod=dba()}).
change_storage(Table,Type) -> change_storage(Table,Type, #kvx{mod=dba()}).
join()             -> join    ([],    #kvx{mod=dba()}).
join(Node)         -> join    (Node,  #kvx{mod=dba()}).
leave()            -> leave   (#kvx{mod=dba()}).
count(Table)       -> count   (Table, #kvx{mod=dba()}).
put(Record)        -> ?MODULE:put     (Record, #kvx{mod=dba()}).
fold(Fun,Acc,T,S,C,D) -> fold (Fun,Acc,T,S,C,D, #kvx{mod=dba()}).
info(T)            -> info    (T, #kvx{mod=dba()}).
start()            -> start   (#kvx{mod=dba()}).
stop()             -> stop    (#kvx{mod=dba()}).
destroy()          -> destroy (#kvx{mod=dba()}).
ver()              -> ver(#kvx{mod=dba()}).
dir()              -> dir     (#kvx{mod=dba()}).
seq(Table,DX)      -> seq     (Table, DX, #kvx{mod=dba()}).

% stream api

top  (X) -> kvx_stream:top (X).
bot  (X) -> kvx_stream:bot (X).
next (X) -> kvx_stream:next(X).
prev (X) -> kvx_stream:prev(X).
drop (X) -> kvx_stream:drop(X).
take (X) -> kvx_stream:take(X).
save (X) -> kvx_stream:save(X).
up   (X) -> kvx_stream:up  (X).
down (X) -> kvx_stream:down(X).
add  (X) -> kvx_stream:add (X).
load_writer (X) -> kvx_stream:load_writer(X).
load_reader (X) -> kvx_stream:load_reader(X).
writer      (X) -> kvx_stream:writer(X).
reader      (X) -> kvx_stream:reader(X).

metainfo() ->  #schema { name = kvx , tables = core() }.
core()    -> [ #table { name = id_seq , fields = record_info(fields,id_seq) , keys=[thing]} ].

init(Backend, Module) ->
    [ begin
        Backend:create_table(T#table.name, [{attributes,T#table.fields},
               {T#table.copy_type, [node()]},{type,T#table.type}]),
        [ Backend:add_table_index(T#table.name, Key) || Key <- T#table.keys ],
        T
    end || T <- (Module:metainfo())#schema.tables ].

all(Tab,#kvx{mod=DBA}) -> DBA:all(Tab).
start(#kvx{mod=DBA}) -> DBA:start().
stop(#kvx{mod=DBA}) -> DBA:stop().
change_storage(Type) -> [ change_storage(Name,Type) || #table{name=Name} <- kvx:tables() ].
change_storage(Table,Type,#kvx{mod=DBA}) -> DBA:change_storage(Table,Type).
destroy(#kvx{mod=DBA}) -> DBA:destroy().
join(Node,#kvx{mod=DBA}) -> DBA:join(Node).
leave(#kvx{mod=DBA}) -> DBA:leave().
ver(#kvx{mod=DBA}) -> DBA:version().
tables() -> lists:flatten([ (M:metainfo())#schema.tables || M <- modules() ]).
table(Name) when is_atom(Name) -> lists:keyfind(Name,#table.name,tables());
table(_) -> false.
dir(#kvx{mod=DBA}) -> DBA:dir().
info(T,#kvx{mod=DBA}) -> DBA:info(T).
modules() -> application:get_env(kvx,schema,[]).
cursors() ->
    lists:flatten([ [ {T#table.name,T#table.fields}
        || #table{name=Name}=T <- (M:metainfo())#schema.tables, Name == reader orelse Name == writer  ]
    || M <- modules() ]).

fold(___,Acc,_,[],_,_,_) -> Acc;
fold(___,Acc,_,undefined,_,_,_) -> Acc;
fold(___,Acc,_,_,0,_,_) -> Acc;
fold(Fun,Acc,Table,Start,Count,Direction,Driver) ->
    kvx:trace("FOLD: ~p~n",[{Table, Start, Driver}]),
    try
    case kvx:get(Table, Start, Driver) of
         {ok, R} -> Prev = element(Direction, R),
                    Count1 = case Count of C when is_integer(C) -> C - 1; _-> Count end,
                    fold(Fun, Fun(R,Acc), Table, Prev, Count1, Direction, Driver);
          _Error -> kvx:error(?MODULE,"Error: ~p~n",[_Error]),
                    Acc
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
seq(Tab, Incr,#kvx{mod=DBA}) -> DBA:seq(case table(Tab) of #table{} -> atom_to_list(Tab); _ -> Tab end, Incr).
notify(_EventPath, _Data) -> skip.

dump() -> dump([ N || #table{name=N} <- kvx:tables() ]), ok.
dump(short) ->
    Gen = fun(T) ->
        {S,M,C}=lists:unzip3([ dump_info(R) || R <- T ]),
        {lists:usort(S),lists:sum(M),lists:sum(C)}
    end,
    dump_format([ {T,Gen(T)} || T <- [ N || #table{name=N} <- kvx:tables() ] ]);
dump(Table) when is_atom(Table) -> dump(Table);
dump(Tables) ->
    dump_format([{T,dump_info(T)} || T <- lists:flatten(Tables) ]).
dump_info(T) ->
    {mnesia:table_info(T,storage_type),
    mnesia:table_info(T,memory) * erlang:system_info(wordsize) / 1024 / 1024,
    mnesia:table_info(T,size)}.
dump_format(List) ->
    io:format("~20s ~32s ~14s ~10s~n~n",["NAME","STORAGE TYPE","MEMORY (MB)","ELEMENTS"]),
    [ io:format("~20s ~32w ~14.2f ~10b~n",[T,S,M,C]) || {T,{S,M,C}} <- List ],
    io:format("~nSnapshot taken: ~p~n",[calendar:now_to_datetime(os:timestamp())]).

logger()       -> application:get_env(?MODULE,logger,n2o_io).
log_modules()  -> application:get_env(?MODULE,log_modules,[]).
log_level()    -> application:get_env(?MODULE,log_level,info).

level(none)    -> 4;
level(error)   -> 3;
level(warning) -> 2;
level(trace)   -> 1;
level(_)       -> 0.

log(M,F,A,Fun) ->
    case level(Fun) < level(log_level()) of
         true  -> skip;
         false -> case    log_modules() of
             any       -> (logger()):Fun(M,F,A);
             []        -> skip;
             Allowed   -> case lists:member(M, Allowed) of
                 true  -> (logger()):Fun(M,F,A);
                 false -> skip end end end.

info   (Module, String, Args) -> log(Module,  String, Args, info).
trace  (Module, String, Args) -> log(Module,  String, Args, trace).
warning(Module, String, Args) -> log(Module,  String, Args, warning).
error  (Module, String, Args) -> log(Module,  String, Args, error).

% tests

check() ->
    Id  = {list},
    X   = 5,
    _W   = kvx:save(kvx:writer(Id)),
    #reader{id=R1} = kvx:save(kvx:reader(Id)),
    #reader{id=R2} = kvx:save(kvx:reader(Id)),
    [ kvx:save(kvx:add((kvx:load_writer(Id))
      #writer{args=#emails{}})) || _ <- lists:seq(1,X) ],
    Bot = kvx:bot(kvx:load_reader(R1)),
    Top = kvx:top(kvx:load_reader(R2)),
    #reader{args=F} = kvx:take(Bot#reader{args=20,dir=0}),
    #reader{args=B} = kvx:take(Top#reader{args=20,dir=1}),
    ?assertMatch(X,length(F)),
    ?assertMatch(F,lists:reverse(B)).
