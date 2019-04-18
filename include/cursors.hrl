-ifndef(CURSORS_HRL).
-define(CURSORS_HRL, true).

-record(writer, { id    = [] :: term(),
                  count =  0 :: integer(),
                  cache = [] :: [] | tuple(),
                  args  = [] :: term(),
                  first = [] :: [] | tuple() } ).
-record(reader, { id    = [] :: integer(),
                  pos   =  0 :: [] | integer(),
                  cache = [] :: [] | integer(),
                  args  = [] :: term(),
                  feed  = [] :: term(),
                  dir   =  0 :: 0 | 1 } ).
-endif.