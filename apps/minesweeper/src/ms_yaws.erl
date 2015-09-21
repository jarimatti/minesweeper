%%%-------------------------------------------------------------------
%%% @author Jarimatti Valkonen <jarimatti@iki.fi>
%%% @copyright (C) 2015, Jarimatti Valkonen
%%% @doc
%%% Minesweeper Yaws appmod user interface.
%%% @end
%%%-------------------------------------------------------------------
-module(ms_yaws).
-author("Jarimatti Valkonen <jarimatti@iki.fi>").

-include("../../../deps/yaws/include/yaws_api.hrl").

-export([out/1]).

%%%===================================================================
%%% API
%%%===================================================================

out(A) ->
    Path = A#arg.pathinfo,
    Req = A#arg.req,
    Method = Req#http_request.method,
    out(Method, Path, A).

out('GET', "/status", _A) ->
    {ehtml, "Status: OK"};
out('POST', "/new", _A) ->
    start_new_game();
out('POST', "/open", A) ->
    {ok, {Row, Col}} = decode_row_col_input(A#arg.clidata),
    Pos = ms_server:make_pos(Row, Col),
    handle_with_session(fun (Pid) ->
                                ms_server:open(Pid, Pos)
                        end,
                        A);
out('POST', "/mark", A) ->
    {ok, {Row, Col}} = decode_row_col_input(A#arg.clidata),
    Pos = ms_server:make_pos(Row, Col),
    handle_with_session(fun (Pid) ->
                                ms_server:flag(Pid, Pos)
                        end,
                        A);
out('POST', "/unmark", A) ->
    {ok, {Row, Col}} = decode_row_col_input(A#arg.clidata),
    Pos = ms_server:make_pos(Row, Col),
    handle_with_session(fun (Pid) ->
                                ms_server:clear_flag(Pid, Pos)
                        end,
                        A);
out(_Method, _Path, _A) ->
    {status, 404}.

%% Call Fun with Minesweeper Pid, handle resulting UI state.
handle_with_session(Fun, A) ->
    H = A#arg.headers,
    C = H#headers.cookie,
    case yaws_api:find_cookie_val("minesweeper", C) of
        [] ->
            {status, 400};
        Cookie ->
            case yaws_api:cookieval_to_opaque(Cookie) of
                {ok, Pid} ->
                    {ok, State} = Fun(Pid),
                    json_content(ui_state_to_json(State));
                {error, no_session} ->
                    {status, 400}
            end
    end.
    

%% Produce the data of new game.
%% CAUTION: Adds a cookie into the Yaws cookie session as side effect.
start_new_game() ->
    {ok, Pid} = minesweeper_sup:start_child(10, 10, 10),
    Cookie = yaws_api:new_cookie_session(Pid),
    CO = yaws_api:set_cookie("minesweeper", Cookie, [{path, "/"}]),
    {ok, State} = ms_server:get_board(Pid),
    [json_content(ui_state_to_json(State)), CO].

%% Produce {Row, Col} tuple from 'open' command JSON input.
decode_row_col_input(Data) ->
    String = binary:bin_to_list(Data),
    {ok, {struct, [{"row", Row}, {"col", Col}]}} = json2:decode_string(String),
    {ok, {Row, Col}}.

%% Produce Yaws API response contaning the JSON data.
json_content(Output) ->
    {content, "application/json; charset=iso-8559-1", Output}.

%% Produce a JSON from UI state.
ui_state_to_json({State, Board}) ->
    StateString = format_state(State),
    BoardArray = format_board(Board),
    json2:encode({struct,
                  [{state, StateString},
                   {board, BoardArray}]}).

%% Produce a string of the game state.
format_state(continue) ->
    "continue";
format_state(victory) ->
    "victory";
format_state(defeat) ->
    "defeat".

%% Produce a JSON struct representation from the UI board.
format_board({Rows, Cols, Data}) ->
    {struct, [{rows, Rows},
              {cols, Cols},
              {data, format_data(Data)}]}.

%% Produce an array of strings from the UI board cells.
format_data(Data) ->
    lists:map(fun format_cell/1, Data).

%% Produce a JSON encodable representation of an UI cell.
format_cell(empty) ->
    "empty";
format_cell(flag) ->
    "flag";
format_cell(false_flag) ->
    "false_flag";
format_cell(mine) ->
    "mine";
format_cell(hidden) ->
    "hidden";
format_cell(Mines) when is_integer(Mines) ->
    Mines.
