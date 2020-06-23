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
play(Pid, State = {continue, Board}) ->
    show_board(Board),
    case get_input() of
        quit ->
            ms_server:quit(Pid),
            {ok, quit};
        print ->
            play(Pid, State);
        {open, Pos} ->
            {ok, NewState} = ms_server:open(Pid, Pos),
            play(Pid, NewState);
        {flag, Pos} ->
            {ok, NewState} = ms_server:flag(Pid, Pos),
            play(Pid, NewState);
        {unflag, Pos} ->
            {ok, NewState} = ms_server:clear_flag(Pid, Pos),
            play(Pid, NewState)
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
%%
%% This need not be particularly efficient, hence the list appending,
%% flatmapping, flattening are acceptable.
-spec format_board(ms_server:ui_board()) -> string().
format_board(Board = {Rows, Cols, _Data}) ->
    RowDigits = digits(Rows),
    Header = format_column_headers(Cols, RowDigits),
    Footer = format_board_footer(Cols, RowDigits),
    Data = lists:flatmap(fun (Row) ->
                                 RowHeader = format_row_header(Row, RowDigits),
                                 RowData = format_row_data(Row, Board),
                                 RowHeader ++ "|" ++ RowData ++ "|\n"
                         end,
                         lists:seq(1, Rows)),
    Header ++ Footer ++ Data ++ Footer.

-ifdef('TEST').

format_board_test() ->
    Board = {2, 3, [0, 1, hidden,
                    0, 1, flag]},
    Expected = "  123\n +---+\n1|.1#|\n2|.1x|\n +---+\n",
    Actual = format_board(Board),
    ?assertEqual(Expected, Actual).

-endif.

format_column_headers(Cols, RowDigits) ->
    ColDigits = digits(Cols),
    Spaces = lists:duplicate(RowDigits + 1, $ ),
    ColRowNumbers =
        fun (Scaler) ->
                fun (Col) ->
                        Rem = trunc(math:pow(10, Scaler)),
                        Div = trunc(math:pow(10, Scaler - 1)),
                        (Col rem Rem) div Div
                end
        end,
    FormatColRow =
        fun (Scaler) ->
                Digits = lists:map(ColRowNumbers(Scaler),
                                   lists:seq(1, Cols)),
                Data = column_header_row(Digits),
                Spaces ++ Data ++ "\n"
        end,
    lists:flatmap(FormatColRow,
                  lists:reverse(lists:seq(1, ColDigits))).

-ifdef('TEST').

format_column_headers_test_() ->
    [?_assertEqual("  12\n", format_column_headers(2, 1)),
     ?_assertEqual("           11\n  12345678901\n", format_column_headers(11, 1))].

-endif.

%% Produce a string from list of digits, replacing initial 0 as spaces.
-spec column_header_row([non_neg_integer()]) -> string().
column_header_row(X) ->
    column_header_row(X, initial_zeroes, "").

column_header_row([], _, Acc) ->
    Acc;
column_header_row([0 | Rest], initial_zeroes, Acc) ->
    column_header_row(Rest, initial_zeroes, Acc ++ " ");
column_header_row([X | Rest], _, Acc) ->
    Digit = lists:flatten(io_lib:format("~B", [X])),
    column_header_row(Rest, non_zeroes, Acc ++ Digit).

%% Produce a row footer, given the amount of Cols and digits for rows.
format_board_footer(Cols, RowDigits) ->
    Spaces = lists:duplicate(RowDigits, $ ),
    Bar = lists:duplicate(Cols, $-),
    Spaces ++ "+" ++ Bar ++ "+\n".

-ifdef('TEST').

format_board_footer_test() ->
    Expected = "  +----+\n",
    ?assertEqual(Expected, format_board_footer(4, 2)).

-endif.

%% Produce a string from a row in the board.
-spec format_row_data(ms_server:row(), ms_server:ui_board()) -> string().
format_row_data(Row, Board = {_Rows, Cols, _Data}) ->
    lists:flatmap(fun (Col) ->
                          Cell = board_cell(Board, Row, Col),
                          format_cell(Cell)
                  end,
                  lists:seq(1, Cols)).

-ifdef('TEST').

format_row_data_test() ->
    Row = 1,
    Board = {3, 3, [0, 2, hidden,
                    0, 2, mine,
                    0, 1, 1]},
    ?assertEqual(".2#", format_row_data(Row, Board)).

-endif.

%% Produce a row header, without separator character.
-spec format_row_header(ms_server:row(), non_neg_integer()) -> string().
format_row_header(Row, RowDigits) ->
    lists:flatten(io_lib:format("~*B", [RowDigits, Row])).

-ifdef('TEST').

format_row_header_test_() ->
    [?_assertEqual("2", format_row_header(2, 1)),
     ?_assertEqual(" 5", format_row_header(5, 2)),
     ?_assertEqual("13", format_row_header(13, 2))].

-endif.

%% Produce the number of digits in the number.
-spec digits(non_neg_integer()) -> non_neg_integer().
digits(X) when X > 0 ->
    trunc(math:log10(X)) + 1.

-ifdef('TEST').

digits_test_() ->
    [?_assertEqual(1, digits(1)),
     ?_assertEqual(1, digits(9)),
     ?_assertEqual(2, digits(10)),
     ?_assertEqual(2, digits(19))].

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
-spec get_input() -> {flag | open | unflag, ms_server:pos()} | print | quit.
get_input() ->
    {ok, Command, Params} =
        io_lib:fread(
          "~s",
          io:get_line("[u]nflag / [f]lag / [o]pen r c or [q]uit: ")),
    case hd(Command) of
        "f" ->
            get_input_pos(flag, Params);
        "o" ->
            get_input_pos(open, Params);
        "u" ->
            get_input_pos(unflag, Params);
        "p" ->
            print;
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
    io:format("  p       display the board again~n"),
    io:format("  q       quits the game~n").


%% Produce a cell in Board from Row, Col.
-spec board_cell(ms_server:ui_board(), ms_server:row(), ms_server:col()) -> ms_server:ui_cell().
board_cell({_Rows, Cols, Data}, Row, Col) ->
    lists:nth((Row - 1) * Cols + Col, Data).
