-module(minesweeper_app).

-behaviour(application).

-export([start/2, stop/1]).


-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
                 [{'_',
                   [{
                      "/api/:method",
                      [{method, function, fun ms_cowboy:validate_method/1}],
                      ms_cowboy,
                      []
                    },
                    {
                      "/",
                      cowboy_static,
                      {priv_file, minesweeper, "html/dynamic_sample.html"}
                    },
                    {
                      "/minesweeper_tiles.svg",
                      cowboy_static,
                      {priv_file, minesweeper, "html/minesweeper_tiles.svg"}
                    }]
                  }]
                ),
    {ok, _} = cowboy:start_http(
                minesweeper_http_listener,
                100,
                [{port, 8080}],
                [{env, [{dispatch, Dispatch}]}]
               ),
    minesweeper_sup:start_link().


stop(_State) ->
    ok = cowboy:stop_listener(minesweeper_http_listener),
    ok.
