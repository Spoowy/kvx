-ifndef(STREAM_HRL).
-define(STREAM_HRL, true).
-include("kvx.hrl").
-include("cursors.hrl").
-define(STREAM, [top/1, bot/1, next/1, prev/1, drop/1, take/1, append/2, cut/2,
                 load_writer/1, load_reader/1, writer/1, reader/1, save/1, add/1]).
-spec top(#reader{}) -> #reader{}.
-spec bot(#reader{}) -> #reader{}.
-spec next(#reader{}) -> #reader{}.
-spec prev(#reader{}) -> #reader{}.
-spec drop(#reader{}) -> #reader{}.
-spec take(#reader{}) -> #reader{}.
-spec load_writer (term()) -> {ok,term()} | {error,term()}.
-spec load_reader (integer()) -> {ok,term()} | {error,term()}.
-spec writer (term()) -> #writer{}.
-spec reader (term()) -> #reader{}.
-spec save (#reader{} | #writer{}) -> #reader{} | #writer{}.
-spec add(#writer{}) -> #writer{}.
-spec append(tuple(),term()) -> term().
-spec cut(term(),term()) -> term().
-endif.
