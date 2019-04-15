-ifndef(KVX_HRL).
-define(KVX_HRL, true).
-record(id_seq, { thing = [] :: term(),
                  id    =  0 :: integer() } ).
-record(it,     { id    = [] :: [] | integer() } ).
-record(iter,   { id    = [] :: [] | integer(),
                  next  = [] :: [] | integer(),
                  prev  = [] :: [] | integer() } ).
-record(emails, { id    = [] :: [] | integer(),
                  next  = [] :: [] | integer(),
                  prev  = [] :: [] | integer(),
                  email = [] :: [] | binary() }).
-record(kvx,    { mod   = kvx_mnesia :: kvx_mnesia | kvx_rocks | kvx_fs,
                  cx    = [] :: term() }).
-endif.
