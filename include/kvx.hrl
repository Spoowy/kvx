-ifndef(KVX_HRL).
-define(KVX_HRL, true).
-record(id_seq, { thing = []::term(), id =  0 :: integer() } ).
-record(it,     { id    = []::[] | integer() } ).
-record(ite,    { id    = []::[] | integer(), next  = []::[] | integer() } ).
-record(iter,   { id    = []::[] | integer(), next  = []::[] | integer(), prev  = []::[] | integer() } ).
-record(kvx,    { mod   = kvx_mnesia :: kvx_mnesia | kvx_rocks | kvx_fs,
                  st    = kvx_stream :: kvx_stream | kvx_st,
                  cx    = []::term() }).
-endif.
