-module(minesweeper_app).

-behaviour(application).

-export([start/2, stop/1]).


-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    {ok, _} = cowboy:start_clear(
                minesweeper_http_listener,
                [{port, 8080}],
		#{env => #{dispatch => dispatch()}}
               ),
    minesweeper_sup:start_link().


stop(_State) ->
    ok = cowboy:stop_listener(minesweeper_http_listener),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions.

%% Produce compiled Cowboy dispatch rules for Minesweeper.
-spec dispatch() -> cowboy_router:dispatch_rules().
dispatch() ->
    cowboy_router:compile(
      [{'_',
        [{
           "/api/:action",
           [{action, fun ms_cowboy:validate_action/2}],
           ms_cowboy,
           []
         },
         {
           "/",
           cowboy_static,
           {priv_file, minesweeper, "html/minesweeper.html"}
         },
         {
           "/minesweeper.svg",
           cowboy_static,
           {priv_file, minesweeper, "html/minesweeper.svg"}
         }]
       }]
     ).
