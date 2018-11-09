-ifndef(STREAM_HRL).
-define(STREAM_HRL, true).
-define(STREAM, [top/1, bot/1, next/1, prev/1, drop/1, take/1,
                 load_writer/1, load_reader/1, writer/1, reader/1,
                 save/1, up/1, down/1, add/1, metainfo/0, check/0]).
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
-spec up   (#reader{}) -> #reader{}.
-spec down (#reader{}) -> #reader{}.
-spec add(#writer{}) -> #writer{}.
-endif.
