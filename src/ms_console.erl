%%%-------------------------------------------------------------------
%%% @author Jarimatti <jarimatti@iawe.local>
%%% @copyright (C) 2014, Jarimatti
%%% @doc
%%% Minesweeper console interface.
%%% @end
%%% Created : 16 Oct 2014 by Jarimatti <jarimatti@iawe.local>
%%%-------------------------------------------------------------------
-module(ms_console).

-export([start_game/3]).

-ifdef('TEST').
-include_lib("eunit/include/eunit.hrl").
-endif.


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% GameResult is one of
%% - victory
%% - defeat
%% - quit
%% interp. victory = user won the game
%%         defeat  = user lost the game
%%         quit    = user quit the game
-type game_result() :: victory | defeat | quit.


%% Start a new console game of minesweeper with Rows, Cols and Prob of mines.
%%
%% Tip: the Prob of mines should be kept relatively small, otherwise
%% the game will be impossible.
-spec start_game(ms_server:row(), ms_server:col(), ms_server:mine_prob()) -> {ok, game_result()}.
start_game(R, C, P) ->
    {ok, Game} = ms_server:start_link(R, C, P),
    {ok, State} = ms_server:get_board(Game),
    play(Game, State).


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% Play the game until quit, defeated or won.
%% Caution: side effects.
-spec play(pid(), ms_server:ui_state()) -> {ok, victory | defeat | quit}.
play(Pid, {continue, Board}) ->
    show_board(Board),
    case get_input() of
        quit ->
            ms_server:quit(Pid),
            {ok, quit};
        {open, Pos} ->
            {ok, State} = ms_server:open(Pid, Pos),
            play(Pid, State);
        {flag, Pos} ->
            {ok, State} = ms_server:flag(Pid, Pos),
            play(Pid, State);
        {unflag, Pos} ->
            {ok, State} = ms_server:clear_flag(Pid, Pos),
            play(Pid, State)
    end;
play(Pid, {victory, Board}) ->
    show_board(Board),
    io:format("Victory!~n"),
    ms_server:quit(Pid),
    {ok, victory};
play(Pid, {defeat, Board}) ->
    show_board(Board),
    io:format("Defeat.~n"),
    ms_server:quit(Pid),
    {ok, defeat}.



%% Print the visible cells in the board.
-spec show_board(ms_server:ui_board()) -> ok.
show_board(Board) ->
    io:format(format_board(Board)).

%% Produce a multi-line string representation of the visible board.
-spec format_board(ms_server:ui_board()) -> string().
format_board(Board = {Rows, _Cols, _Data}) ->
    lists:flatmap(fun (Row) ->
                          format_line(Row, Board) ++ "\n"
                  end,
                  lists:seq(1, Rows)).

-ifdef('TEST').

format_board_test() ->
    Board = {2, 3, [0, 1, hidden,
                    0, 1, flag]},
    Expected = ".1#\n.1x\n",
    Actual = format_board(Board),
    ?assertEqual(Expected, Actual).

-endif.

%% Produce a string from a row in the board.
-spec format_line(ms_server:row(), ms_server:ui_board()) -> string().
format_line(Row, Board = {_Rows, Cols, _Data}) ->
    lists:flatmap(fun (Col) ->
                          Cell = board_cell(Board, Row, Col),
                          format_cell(Cell)
                  end,
                  lists:seq(1, Cols)).

-ifdef('TEST').

format_line_test() ->
    Row = 1,
    Board = {3, 3, [0, 2, hidden,
                    0, 2, mine,
                    0, 1, 1]},
    ?assertEqual(".2#", format_line(Row, Board)).

-endif.

%% Produce a string from a cell in the board.
%% # = invisible
%% * = visible mine = BOOM!
%% . = no mines in neighbouring cells
%% 4 = 4 mines in neighbouring cells
%% x = flag in cell
%% o = false flag in cell (flagged, but not mine) after game over.
-spec format_cell(ms_server:ui_cell()) -> string().
format_cell(hidden) ->
    "#";
format_cell(mine) ->
    "*";
format_cell(flag) ->
    "x";
format_cell(false_flag) ->
    "o";
format_cell(0) ->
    ".";
format_cell(Count) ->
    io_lib:write(Count).

-ifdef('TEST').

format_cell_test_() ->
    [?_assertEqual("#", format_cell(hidden)),
     ?_assertEqual("*", format_cell(mine)),
     ?_assertEqual("x", format_cell(flag)),
     ?_assertEqual("o", format_cell(false_flag)),
     ?_assertEqual(".", format_cell(0)),
     ?_assertEqual("3", format_cell(3))].

-endif.

%% Produce an Input asked from user.
%% Caution: side effects.
%% Tested manually.
-spec get_input() -> {flag | open | unflag, ms_server:pos()} | quit.
get_input() ->
    {ok, Command, Params} = io_lib:fread(
                              "~s",
                              io:get_line("[u]nflag / [f]lag / [o]pen r c or [q]uit: ")),
    case hd(Command) of
        "f" ->
            get_input_pos(flag, Params);
        "o" ->
            get_input_pos(open, Params);
        "u" ->
            get_input_pos(unflag, Params);
        "q" ->
            quit;
        _ ->
            print_help(),
            get_input()
    end.


%% Ask for position, produce user input.
%% Mutually recursive with user_input.
-spec get_input_pos(flag | open | unflag, string()) -> {flag | open | unflag, ms_server:pos()}.
get_input_pos(Command, Data) ->
    case io_lib:fread("~d ~d", Data) of
        {ok, [Row, Col], _Rest} ->
            {Command, ms_server:make_pos(Row, Col)};
        _ ->
            print_help(),
            get_input()
    end.


%% Display a help message to user.
%% Caution: side effects.
-spec print_help() -> ok.
print_help() ->
    io:format("Commands:~n"),
    io:format("  f 2 3   flags cell at row 2 column 3 as a mine~n"),
    io:format("  u 2 3   clears flag from cell at row 2 column 3~n"),
    io:format("  o 2 3   opens cell at row 2 column 3~n"),
    io:format("  q       quits the game~n").


%% Produce a cell in Board from Row, Col.
-spec board_cell(ms_server:ui_board(), ms_server:row(), ms_server:col()) -> ms_server:ui_cell().
board_cell({_Rows, Cols, Data}, Row, Col) ->
    lists:nth((Row - 1) * Cols + Col, Data).
