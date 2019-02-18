-module(kvx_sup).
-behaviour(application).
-behaviour(supervisor).
-export([init/1, start_link/0, start/2, stop/1]).
start_link() -> supervisor:start_link({local, kvx_sup}, kvx_sup, []).
init([]) -> {ok, { {one_for_one, 5, 10}, []} }.
start(_,_) -> kvx_sup:start_link().
stop(_) -> ok.
