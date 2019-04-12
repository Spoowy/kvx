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
    Address = <<(list_to_binary(lists:concat(["/",Tab,"/"])))/binary,(term_to_binary(Key))/binary>>,
    case rocksdb:get(ref(), Address, []) of
         not_found -> {error,not_found};
         {ok,Bin} -> {ok,binary_to_term(Bin,[safe])} end.

put(Records) when is_list(Records) -> lists:map(fun(Record) -> put(Record) end, Records);
put(Record) -> rocksdb:put(ref(), <<(list_to_binary(lists:concat(["/",element(1,Record),"/"])))/binary,
                                    (term_to_binary(element(2,Record)))/binary>>, term_to_binary(Record), [{sync,true}]).

delete(_Tab, _Key) -> ok.
count(RecordName) -> {ok,I} = rocksdb:iterator(ref(), []), [].
all(R) -> {ok,I} = rocksdb:iterator(ref(), []),
           Key = list_to_binary(lists:concat(["/",R])),
           First = rocksdb:iterator_move(I, {seek,Key}),
           lists:reverse(next(I,Key,size(Key),First,[],[])).

next(I,Key,S,{ok,A,X},_,T) -> next(I,Key,S,A,X,T);
next(I,Key,S,{error,_},_,T) -> T;
next(I,Key,S,A,X,T) ->
     case binary:part(A,0,S) of Key ->
          next(I,Key,S,rocksdb:iterator_move(I, next), [],
                       [binary_to_term(X,[safe])|T]);
                  _ -> T end.

seq(R, I) -> kvx_mnesia:seq(R, I).
create_table(_,_) -> [].
add_table_index(_, _) -> ok.
