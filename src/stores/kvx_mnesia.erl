-module(kvx_mnesia).
-include("backend.hrl").
-include("kvx.hrl").
-include("metainfo.hrl").
-include_lib("mnesia/src/mnesia.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export(?BACKEND).
-export([info/1,exec/1,sync_indexes/0,sync_indexes/1,dump/1]).
start()    -> mnesia:start().
stop()     -> mnesia:stop().
destroy()  -> [mnesia:delete_table(T)||{_,T}<-kvx:dir()], mnesia:delete_schema([node()]), ok.
version()  -> {version,"KVX MNESIA"}.
dir()      -> [{table,T}||T<-mnesia:system_info(local_tables)].
join([])   -> mnesia:start(), mnesia:change_table_copy_type(schema, node(), disc_copies), initialize();
join(Node) ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [Node]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    [{Tb, mnesia:add_table_copy(Tb, node(), Type)}
     || {Tb, [{N, Type}]} <- [{T, mnesia:table_info(T, where_to_commit)}
                               || T <- mnesia:system_info(tables)], Node==N].

%change_storage(Table,Type) -> mnesia:change_table_copy_type(Table, node(), Type).

initialize() ->
    mnesia:create_schema([node()]),
    Res = [ kvx:initialize(kvx_mnesia,Module) || Module <- kvx:modules() ],
    mnesia:wait_for_tables([ T#table.name || T <- kvx:tables()],infinity),
    Res.

index(Tab,Key,Value) ->
    Table = kvx:table(Tab),
    Index = string:str(Table#table.fields,[Key]),
    lists:flatten(many(fun() -> mnesia:index_read(Tab,Value,Index+1) end)).

get(RecordName, Key) -> just_one(fun() -> mnesia:read(RecordName, Key) end).
put(Records) when is_list(Records) -> void(fun() -> lists:foreach(fun mnesia:write/1, Records) end);
put(Record) -> put([Record]).
delete(Tab, Key) ->
    case mnesia:activity(context(),fun()-> mnesia:delete({Tab, Key}) end) of
        {aborted,Reason} -> {error,Reason};
        {atomic,_Result} -> ok;
        _ -> ok end.
count(RecordName) -> mnesia:table_info(RecordName, size).
all(R) -> lists:flatten(many(fun() -> L= mnesia:all_keys(R), [ mnesia:read({R, G}) || G <- L ] end)).
seq([],[]) -> os:system_time();
seq(RecordName, Incr) -> mnesia:dirty_update_counter({id_seq, RecordName}, Incr).
many(Fun) -> case mnesia:activity(context(),Fun) of {atomic, R} -> R; {aborted, Error} -> {error, Error}; X -> X end.
void(Fun) -> case mnesia:activity(context(),Fun) of {atomic, ok} -> ok; {aborted, Error} -> {error, Error}; X -> X end.
info(T) -> try mnesia:table_info(T,all) catch _:_ -> [] end.
create_table(Name,Options) -> mnesia:create_table(Name, Options).
add_table_index(Record, Field) -> mnesia:add_table_index(Record, Field).
exec(Q) -> F = fun() -> qlc:e(Q) end, {atomic, Val} = mnesia:activity(context(),F), Val.
just_one(Fun) ->
    case mnesia:activity(context(),Fun) of
        {atomic, []} -> {error, not_found};
        {atomic, R} -> {ok, R};
        [] -> {error, not_found};
        R when is_list(R) -> {ok,R};
        Error -> Error end.

%add(Record) -> mnesia:activity(context(),fun() -> kvx:append(Record,#kvx{mod=?MODULE}) end).
context() -> application:get_env(kvx,mnesia_context,async_dirty).

sync_indexes() ->
    lists:map(fun sync_indexes/1, kvx:tables()).
sync_indexes(#table{name = Table, keys = Keys}) ->
    mnesia:wait_for_tables(Table, 10000),
    #cstruct{attributes = Attrs} = mnesia:table_info(Table, cstruct),
    Indexes = mnesia:table_info(Table, index),
    IndexedKeys = [lists:nth(I-1, Attrs)|| I <- Indexes],
    [mnesia:del_table_index(Table, Key) || Key <- IndexedKeys -- Keys],
    [mnesia:add_table_index(Table, Key) || Key <- Keys -- IndexedKeys].

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
