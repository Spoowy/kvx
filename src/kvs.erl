-module(kvs).
-description('KVS: Abstract Chain Storage').
-include("api.hrl").
-include("metainfo.hrl").
-include("kvs.hrl").
-export([info/3,warning/3,error/3,trace/3]).
-export(?API).
dba()              -> application:get_env(kvs,dba,store_mnesia).
all(Table)         -> all     (Table, #kvs{mod=dba()}).
delete(Table,Key)  -> delete  (Table, Key, #kvs{mod=dba()}).
get(Table,Key)     -> ?MODULE:get     (Table, Key, #kvs{mod=dba()}).
index(Table,K,V)   -> index   (Table, K,V, #kvs{mod=dba()}).
change_storage(Table,Type) -> change_storage(Table,Type, #kvs{mod=dba()}).
join()             -> join    ([],    #kvs{mod=dba()}).
join(Node)         -> join    (Node,  #kvs{mod=dba()}).
count(Table)       -> count   (Table, #kvs{mod=dba()}).
put(Record)        -> ?MODULE:put     (Record, #kvs{mod=dba()}).
fold(Fun,Acc,T,S,C,D) -> fold (Fun,Acc,T,S,C,D, #kvs{mod=dba()}).
info(T)            -> info    (T, #kvs{mod=dba()}).
start()            -> start   (#kvs{mod=dba()}).
stop()             -> stop    (#kvs{mod=dba()}).
destroy()          -> destroy (#kvs{mod=dba()}).
ver()              -> ver(#kvs{mod=dba()}).
dir()              -> dir     (#kvs{mod=dba()}).
seq(Table,DX)      -> seq     (Table, DX, #kvs{mod=dba()}).

metainfo() ->  #schema { name = kvs , tables = core() }.
core()    -> [ #table { name = id_seq , fields = record_info(fields,id_seq) , keys=[thing]} ].

init(Backend, Module) ->
    [ begin
        Backend:create_table(T#table.name, [{attributes,T#table.fields},
               {T#table.copy_type, [node()]},{type,T#table.type}]),
        [ Backend:add_table_index(T#table.name, Key) || Key <- T#table.keys ],
        T
    end || T <- (Module:metainfo())#schema.tables ].

all(Tab,#kvs{mod=DBA}) -> DBA:all(Tab).
start(#kvs{mod=DBA}) -> DBA:start().
stop(#kvs{mod=DBA}) -> DBA:stop().
change_storage(Type) -> [ change_storage(Name,Type) || #table{name=Name} <- kvs:tables() ].
change_storage(Table,Type,#kvs{mod=DBA}) -> DBA:change_storage(Table,Type).
destroy(#kvs{mod=DBA}) -> DBA:destroy().
join(Node,#kvs{mod=DBA}) -> DBA:join(Node).
ver(#kvs{mod=DBA}) -> DBA:version().
tables() -> lists:flatten([ (M:metainfo())#schema.tables || M <- modules() ]).
table(Name) when is_atom(Name) -> lists:keyfind(Name,#table.name,tables());
table(_) -> false.
dir(#kvs{mod=DBA}) -> DBA:dir().
info(T,#kvs{mod=DBA}) -> DBA:info(T).
modules() -> application:get_env(kvs,schema,[]).
cursors() ->
    lists:flatten([ [ {T#table.name,T#table.fields}
        || #table{name=Name}=T <- (M:metainfo())#schema.tables, Name == reader orelse Name == writer  ]
    || M <- modules() ]).

fold(___,Acc,_,[],_,_,_) -> Acc;
fold(___,Acc,_,undefined,_,_,_) -> Acc;
fold(___,Acc,_,_,0,_,_) -> Acc;
fold(Fun,Acc,Table,Start,Count,Direction,Driver) ->
    kvs:trace("FOLD: ~p~n",[{Table, Start, Driver}]),
    try
    case kvs:get(Table, Start, Driver) of
         {ok, R} -> Prev = element(Direction, R),
                    Count1 = case Count of C when is_integer(C) -> C - 1; _-> Count end,
                    fold(Fun, Fun(R,Acc), Table, Prev, Count1, Direction, Driver);
          _Error -> kvs:error(?MODULE,"Error: ~p~n",[_Error]),
                    Acc
    end catch _:_ -> Acc end.

seq_gen() ->
    Init = fun(Key) ->
           case kvs:get(id_seq, Key) of
                {error, _} -> {Key,kvs:put(#id_seq{thing = Key, id = 0})};
                {ok, _} -> {Key,skip} end end,
    [ Init(atom_to_list(Name))  || {Name,_Fields} <- cursors() ].


put(Records,#kvs{mod=Mod}) when is_list(Records) -> Mod:put(Records);
put(Record,#kvs{mod=Mod}) -> Mod:put(Record).
get(RecordName, Key, #kvs{mod=Mod}) -> Mod:get(RecordName, Key).
delete(Tab, Key, #kvs{mod=Mod}) -> Mod:delete(Tab, Key).
count(Tab,#kvs{mod=DBA}) -> DBA:count(Tab).
index(Tab, Key, Value,#kvs{mod=DBA}) -> DBA:index(Tab, Key, Value).
seq(Tab, Incr,#kvs{mod=DBA}) -> DBA:seq(case table(Tab) of #table{} -> atom_to_list(Tab); _ -> Tab end, Incr).
notify(_EventPath, _Data) -> skip.

dump() -> dump([ N || #table{name=N} <- kvs:tables() ]).
dump(short) ->
    Gen = fun(T) ->
        {S,M,C}=lists:unzip3([ dump_info(R) || R <- T ]),
        {lists:usort(S),lists:sum(M),lists:sum(C)}
    end,
    dump_format([ {T,Gen(T)} || T <- [ N || #table{name=N} <- kvs:tables() ] ]);
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
