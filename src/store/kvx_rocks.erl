-module(kvx_rocks).
-include("backend.hrl").
-include("kvx.hrl").
-include("metainfo.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export(?BACKEND).
start()    -> ok.
stop()     -> ok.
destroy()  -> ok.
version()  -> {version,"KVX ROCKSDB"}.
dir()      -> [].
leave() -> rocksdb:close(application:get_env(kvx,rocks_ref,[])).
join(_) -> {ok, Ref} = rocksdb:open(application:get_env(kvx,rocks_name,"rocksdb"), [{create_if_missing, true}]),
           application:set_env(kvx,rocks_ref,Ref).
change_storage(_,_) -> ok.
initialize() -> [ kvx:init(kvx_rocks,Module) || Module <- kvx:modules() ].
ref() -> application:get_env(kvx,rocks_ref,[]).
index(_,_,_) -> ok.
get(Tab, Key) ->
    Address = list_to_binary(lists:concat(["/",Tab,"/",Key])),
    case rocksdb:get(ref(), Address, []) of
         not_found -> {error,not_found};
         {ok,Bin} -> binary_to_term(Bin,[safe]) end.

put(Records) when is_list(Records) -> lists:map(fun(Record) -> put(Record) end, Records);
put(Record) -> rocksdb:put(ref(), list_to_binary(lists:concat(["/",element(1,Record),"/",
                                  element(2,Record)])), term_to_binary(Record), [{sync,true}]).

delete(_Tab, _Key) -> ok.
count(RecordName) -> {ok,I} = rocksdb:iterator(ref(), []), [].
all(R) -> {ok,I} = rocksdb:iterator(ref(), []),
           First = rocksdb:iterator_move(I, {seek,list_to_binary(lists:concat(["/",R,"/"]))}),
           next(I,First,[]).

next(I,{ok,_,X},T) -> next(I,rocksdb:iterator_move(I, next), [binary_to_term(X,[safe])|T]);
next(I,_,T) -> T.
seq(RecordName, Incr) -> [].
create_table(_,_) -> [].
add_table_index(_, _) -> ok.
