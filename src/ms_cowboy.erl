-module(ms_cowboy).
-behaviour(cowboy_http_handler).

%% Cowboy HTTP callbacks
-export([init/3, handle/2, terminate/3]).

%% Other public functions
-export([validate_action/1]).


%%==============================================================================
%% Data Definitions

%% No Cowboy state.
-type state() :: no_state.

%% Action of the client.
-type action() :: status | new | open | mark | unmark.

%% Session ID.
-type session_id() :: integer().


%%==============================================================================
%% Public Functions

%%------------------------------------------------------------------------------
%% Cowboy HTTP handler callbacks

-spec init(any(), cowboy_req:req(), []) -> {ok, cowboy_req:req(), state()}.
init(_, Req, []) ->
    {ok, Req, no_state}.


-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State = no_state) ->
    {Action, Req2} = cowboy_req:binding(action, Req),
    {Method, Req3} = cowboy_req:method(Req2),
    {SessionID, Req4} = session_id(Req3),
    method(SessionID, Method, Action, Req4, State).


terminate(_Reason, _Req, no_state) ->
    ok.


%%------------------------------------------------------------------------------
%% Other public functions

%% Produce {true, Action} if the action is valid, false otherwise.
-spec validate_action(binary()) -> {true, action()} | false.
validate_action(<<"status">>) ->
    {true, status};
validate_action(<<"new">>) ->
    {true, new};
validate_action(<<"open">>) ->
    {true, open};
validate_action(<<"mark">>) ->
    {true, mark};
validate_action(<<"unmark">>) ->
    {true, unmark};
validate_action(_) ->
    false.


%%==============================================================================
%% Private Functions


%% 
-spec method(pid() | undefined,
             binary(),
             undefined | action(),
             cowboy_req:req(),
             state()) ->
                    {ok, cowboy_req:req(), state()}.
method(_SessionID, <<"GET">>, status, Req, no_state) ->
    {ok, Req2} = cowboy_req:reply(
                   200,
                   [text_plain()],
                   <<"Status: OK">>,
                   Req),
    {ok, Req2, no_state};
method(SessionID, <<"POST">>, new, Req, no_state) ->
    ok = stop_game(SessionID),
    {NewSession, Pid} = start_new_game(),
    Req2 = set_session_id(NewSession, Req),
    {ok, State} = ms_server:get_board(Pid),
    {ok, Req3} = reply(200, State, Req2),
    {ok, Req3, no_state};
method(SessionID, <<"POST">>, open, Req, no_state) ->
    {ok, {Row, Col}, Req2} = parse_row_col(Req),
    {ok, Game} = ms_session:get(SessionID),
    {ok, Req3} = method_with_session(Game, Req2, fun (G) -> ms_server:open(G, ms_server:make_pos(Row, Col)) end),
    {ok, Req3, no_state};
method(SessionID, <<"POST">>, mark, Req, no_state) ->
    {ok, {Row, Col}, Req2} = parse_row_col(Req),
    {ok, Game} = ms_session:get(SessionID),
    {ok, Req3} = method_with_session(Game, Req2, fun (G) -> ms_server:flag(G, ms_server:make_pos(Row, Col)) end),
    {ok, Req3, no_state};
method(SessionID, <<"POST">>, unmark, Req, no_state) ->
    {ok, {Row, Col}, Req2} = parse_row_col(Req),
    {ok, Game} = ms_session:get(SessionID),
    {ok, Req3} = method_with_session(Game, Req2, fun (G) -> ms_server:clear_flag(G, ms_server:make_pos(Row, Col)) end),
    {ok, Req3, no_state};
method(_, _, _, Req, no_state) ->
    {ok, Req2} = cowboy_req:reply(
                   400,
                   [text_plain()],
                   <<"Illegal API method.">>,
                   Req),
    {shutdown, Req2, no_state}.


%% Produce tuple {<<"content-type">>, <<"application/json">>}.
-spec application_json() -> {binary(), binary()}.
application_json() ->
    content_type(<<"application/json">>).


%% Produce tuple {<<"content-type">>, <<"text/plain">>}.
-spec text_plain() -> {binary(), binary()}.
text_plain() ->
    content_type(<<"text-plain">>).


%% Produce tuple {<<"content-type">>, Type}.
-spec content_type(binary()) -> {binary(), binary()}.
content_type(Type) when is_binary(Type) ->
    {<<"content-type">>, Type}.


%% Produce a state of the new game.
%% SIDE EFFECTS:
%% - Create new session & game.
-spec start_new_game() -> {session_id(), pid()}.
start_new_game() ->
    {ok, Pid} = ms_server_sup:start_child(10, 10, 10),
    {ok, SessionID} = ms_session:new(Pid),    
    {SessionID, Pid}.


%% Stop a game using session ID. Do nothing if session ID is undefined.
-spec stop_game(session_id() | undefined) -> ok.
stop_game(undefined) ->
    ok;
stop_game(SessionID) ->
    {ok, Game} = ms_session:get(SessionID),
    stop_game(SessionID, Game).


%% Stop a game or do nothing if the session did not contain a game.
-spec stop_game(session_id(), undefined | pid()) -> ok.
stop_game(_SessionID, undefined) ->
    ok;
stop_game(SessionID, Pid) ->
    ms_session:remove(SessionID),
    ms_server:quit(Pid).



%% Produce a new response with game state as JSON in body.
-spec reply(non_neg_integer(), ms_server:ui_state(), cowboy_req:req()) ->
                   {ok, cowboy_req:req()}.
reply(Code, State, Req) ->
    cowboy_req:reply(
      Code,
      [application_json()],
      format_json(State),
      Req).


%% Produce a JSON representation of the game state.
-spec format_json(ms_server:ui_state()) -> binary().
format_json({State, Board}) ->
    jsone:encode([{state, State}, {board, format_board(Board)}]).


format_board({Rows, Cols, Data}) ->
    [{rows, Rows},
     {cols, Cols},
     {data, Data}].


-spec session_id(cowboy_req:req()) ->
                        {ms_session:id() | undefined, cowboy_req:req()}.
session_id(Req) ->
    {Cookie, Req2} = cowboy_req:cookie(<<"minesweeper">>, Req),
    SessionID = case Cookie of
                    undefined ->
                        undefined;
                    _ ->
                        binary_to_integer(Cookie)
                end,
    {SessionID, Req2}.


-spec set_session_id(ms_session:id(), cowboy_req:req()) -> cowboy_req:req().
set_session_id(SessionID, Req) ->
    cowboy_req:set_resp_cookie(
      <<"minesweeper">>,
      integer_to_binary(SessionID),
      [],
      Req).


%% Produce {Row, Col} from input JSON binary.
-spec parse_row_col(cowboy_req:req()) ->
                           {ok, {integer(), integer()}, cowboy_req:req()}.
parse_row_col(Req) ->
    {ok, B, Req2} = cowboy_req:body(Req),
    Map = jsone:decode(B),
    Row = maps:get(<<"row">>, Map),
    Col = maps:get(<<"col">>, Map),
    {ok, {Row, Col}, Req2}.


%% Produce 400 error if session is undefined, otherwise handle call.


method_with_session(undefined, Req, _Fun) -> 
    cowboy_req:reply(
      400,
      [text_plain()],
      <<"Illegal API method.">>,
      Req);
method_with_session(Pid, Req, Fun) ->
    {ok, State} = Fun(Pid),
    reply(200, State, Req).
