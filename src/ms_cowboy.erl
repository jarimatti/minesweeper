-module(ms_cowboy).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).
-export([validate_method/1]).

-type state() :: no_state.


-spec init(any(), cowboy_req:req(), []) -> {ok, cowboy_req:req(), state()}.
init(_, Req, []) ->
    {ok, Req, no_state}.

-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State = no_state) ->
    {Method, Req2} = cowboy_req:binding(method, Req),
    method(Method, Req2, State).


-spec method(undefined | status, cowboy_req:req(), state()) ->
                    {ok, cowboy_req:req(), state()}.
method(undefined, Req, no_state) ->
    {ok, Req2} = cowboy_req:reply(
                   400,
                   [{<<"content-type">>, <<"text/plain">>}],
                   <<"Unknown API method.">>,
                   Req),
    {shutdown, Req2, no_state};
method(status, Req, no_state) ->
    {ok, Req2} = cowboy_req:reply(
                   200,
                   [{<<"content-type">>, <<"text/plain">>}],
                   <<"Status: OK">>,
                   Req),
    {ok, Req2, no_state}.


terminate(_Reason, _Req, no_state) ->
    ok.

%%------------------------------------------------------------------------------
%% Produce {true, Method} if the method is valid, false otherwise.
-spec validate_method(binary()) -> {true, status} | false.
validate_method(<<"status">>) ->
    {true, status};
validate_method(_) ->
    false.

