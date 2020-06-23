-module(ms_cowboy).
-behaviour(cowboy_handler).

%% Cowboy HTTP callbacks
-export([init/2, handle/2]).

%% Other public functions
-export([validate_action/2]).


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

-spec init(cowboy_req:req(), []) -> {ok, cowboy_req:req(), state()}.
init(Req, []) ->
    handle(Req, no_state).


-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State = no_state) ->
    Action = cowboy_req:binding(action, Req),
    Method = cowboy_req:method(Req),
    SessionID = session_id(Req),
    method(SessionID, Method, Action, Req, State).


%%------------------------------------------------------------------------------
%% Other public functions

%% Produce {ok, Action} if the action is valid, false otherwise.
-spec validate_action(forward | reverse | format_error, binary()) -> {ok, action()} | {error, invalid_action}.
validate_action(forward, <<"status">>) ->
    {ok, status};
validate_action(forward, <<"new">>) ->
    {ok, new};
validate_action(forward, <<"open">>) ->
    {ok, open};
validate_action(forward, <<"mark">>) ->
    {ok, mark};
validate_action(forward, <<"unmark">>) ->
    {ok, unmark};
validate_action(forward, _) ->
    {error, invalid_action}.


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
    Req2 = cowboy_req:reply(
                   200,
                   text_plain(),
                   <<"Status: OK">>,
                   Req),
    {ok, Req2, no_state};
method(SessionID, <<"POST">>, new, Req, no_state) ->
    ok = stop_game(SessionID),
    {NewSession, Pid} = start_new_game(),
    Req2 = set_session_id(NewSession, Req),
    {ok, State} = ms_server:get_board(Pid),
    Req3 = reply(200, State, Req2),
    {ok, Req3, no_state};
method(SessionID, <<"POST">>, open, Req, no_state) ->
    {ok, {Row, Col}, Req2} = parse_row_col(Req),
    {ok, Game} = ms_session:get(SessionID),
    Req3 = method_with_session(Game, Req2, fun (G) -> ms_server:open(G, ms_server:make_pos(Row, Col)) end),
    {ok, Req3, no_state};
method(SessionID, <<"POST">>, mark, Req, no_state) ->
    {ok, {Row, Col}, Req2} = parse_row_col(Req),
    {ok, Game} = ms_session:get(SessionID),
    Req3 = method_with_session(Game, Req2, fun (G) -> ms_server:flag(G, ms_server:make_pos(Row, Col)) end),
    {ok, Req3, no_state};
method(SessionID, <<"POST">>, unmark, Req, no_state) ->
    {ok, {Row, Col}, Req2} = parse_row_col(Req),
    {ok, Game} = ms_session:get(SessionID),
    Req3 = method_with_session(Game, Req2, fun (G) -> ms_server:clear_flag(G, ms_server:make_pos(Row, Col)) end),
    {ok, Req3, no_state};
method(_, _, _, Req, no_state) ->
    Req2 = cowboy_req:reply(
                   400,
                   text_plain(),
                   <<"Illegal API method.">>,
                   Req),
    {shutdown, Req2, no_state}.


%% Produce map #{content_type => <<"application/json">>}.
-spec application_json() -> #{binary() => binary()}.
application_json() ->
    #{<<"content-type">> => <<"application/json">>}.


%% Produce map #{content_type => <<"text/plain">>}.
-spec text_plain() -> #{binary() => binary()}.
text_plain() ->
    #{<<"content-type">> => <<"text-plain">>}.


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
                   cowboy_req:req().
reply(Code, State, Req) ->
    cowboy_req:reply(
      Code,
      application_json(),
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


-spec session_id(cowboy_req:req()) -> ms_session:id() | undefined.
session_id(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    SessionID = case lists:keyfind(<<"minesweeper">>, 1, Cookies) of
                    false ->
                        undefined;
		    {_, Cookie} ->
                        binary_to_integer(Cookie)
                end,
    SessionID.


-spec set_session_id(ms_session:id(), cowboy_req:req()) -> cowboy_req:req().
set_session_id(SessionID, Req) ->
    cowboy_req:set_resp_cookie(
      <<"minesweeper">>,
      integer_to_binary(SessionID),
      Req).


%% Produce {Row, Col} from input JSON binary.
-spec parse_row_col(cowboy_req:req()) ->
                           {ok, {integer(), integer()}, cowboy_req:req()}.
parse_row_col(Req) ->
    {ok, B, Req2} = cowboy_req:read_body(Req),
    Map = jsone:decode(B),
    Row = maps:get(<<"row">>, Map),
    Col = maps:get(<<"col">>, Map),
    {ok, {Row, Col}, Req2}.


%% Produce 400 error if session is undefined, otherwise handle call.


method_with_session(undefined, Req, _Fun) -> 
    cowboy_req:reply(
      400,
      text_plain(),
      <<"Illegal API method.">>,
      Req);
method_with_session(Pid, Req, Fun) ->
    {ok, State} = Fun(Pid),
    reply(200, State, Req).
