%%%-------------------------------------------------------------------
%%% @author Jarimatti <jarimatti@iawe.local>
%%% @copyright (C) 2014, Jarimatti
%%% @doc
%%% Minesweeper console interface.
%%% @end
%%% Created : 16 Oct 2014 by Jarimatti <jarimatti@iawe.local>
%%%-------------------------------------------------------------------
-module(ms_console).

-export([start_game/3, show_board/2, get_input/0]).

-ifdef('TEST').
-include_lib("eunit/include/eunit.hrl").
-endif.


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% Produce the game result
-spec start_game(minesweeper:row(),
                 minesweeper:col(),
                 minesweeper:mine_prob()) -> minesweeper:result().
start_game(N, M, P) ->
    minesweeper:start_game(N, M, P, ?MODULE).


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% Print the visible cells in the board.
-spec show_board(minesweeper:board(), sets:set(minesweeper:pos())) -> ok.
show_board(Board, Visible) ->
    io:format(format_board(Board, Visible)).

%% Produce a multi-line string representation of the visible board.
-spec format_board(minesweeper:board(), sets:set(minesweeper:pos())) -> string().
format_board(Board = {Rows, _Cols, _Data}, Visible) ->
    lists:flatmap(fun (Row) ->
                          format_line(Row, Board, Visible) ++ "\n"
                  end,
                  lists:seq(1, Rows)).

-ifdef('TEST').

format_board_test() ->
    Board = {2, 3, [empty, empty, empty,
                    empty, empty, mine]},
    Visible = sets:from_list([minesweeper:make_pos(1,1),
                              minesweeper:make_pos(1,2),
                              minesweeper:make_pos(1,3),
                              minesweeper:make_pos(2,1),
                              minesweeper:make_pos(2,2)]),
    Expected = ".11\n.1#\n",
    Actual = format_board(Board, Visible),
    ?assertEqual(Expected, Actual).

-endif.

%% Produce a string from a row in the board.
-spec format_line(minesweeper:row(), minesweeper:board(), sets:set(minesweeper:pos())) -> string().
format_line(Row, Board = {_Rows, Cols, _Data}, Visible) ->
    lists:flatmap(fun (Col) ->
                          Pos = minesweeper:make_pos(Row, Col),
                          Cell = minesweeper:get_cell(Board, Pos),
                          Neighbours = minesweeper:board_neighbours(Board, Pos),
                          Mines = minesweeper:count_mines(Neighbours),
                          Is_Visible = sets:is_element(Pos, Visible),
                          format_cell(Cell, Mines, Is_Visible)
                  end,
                  lists:seq(1, Cols)).

-ifdef('TEST').

format_line_test() ->
    Row = 1,
    Board = {3, 3, [empty, empty, mine,
                    empty, empty, mine,
                    empty, empty, empty]},
    Visible = sets:from_list([{1,1},{1,2},
                              {2,1},{2,2},
                              {3,1},{3,2},{3,3}]),
    ?assertEqual(".2#", format_line(Row, Board, Visible)).

-endif.

%% Produce a string from a cell in the board.
%% # = invisible
%% * = visible mine = BOOM!
%% . = no mines in neighbouring cells
%% 4 = 4 mines in neighbouring cells
-spec format_cell(minesweeper:cell(), integer(), boolean()) -> string().
format_cell(_, _, false) ->
    "#";
format_cell(mine, _, true) ->
    "*";
format_cell(empty, 0, true) ->
    ".";
format_cell(empty, Count, true) when Count > 0 ->
    io_lib:write(Count).

-ifdef('TEST').

format_cell_test_() ->
    [?_assertEqual("#", format_cell(empty, 0, false)),
     ?_assertEqual("#", format_cell(mine, 0, false)),
     ?_assertEqual("#", format_cell(empty, 3, false)),
     ?_assertEqual("#", format_cell(mine, 3, false)),
     ?_assertEqual(".", format_cell(empty, 0, true)),
     ?_assertEqual("3", format_cell(empty, 3, true)),
     ?_assertEqual("*", format_cell(mine, 0, true)),
     ?_assertEqual("*", format_cell(mine, 3, true))].

-endif.

%% Produce an Input asked from user.
%% Caution: side effects.
%% Tested manually.
-spec get_input() -> minesweeper:input().
get_input() ->
    {ok, Command, Params} = io_lib:fread(
                              "~s",
                              io:get_line("[m]ark r c, [o]pen r c or [q]uit: ")),
    case hd(Command) of
        "m" ->
            get_input_pos(mark, Params);
        "o" ->
            get_input_pos(open, Params);
        "q" ->
            quit;
        _ ->
            print_help(),
            get_input()
    end.


%% Ask for position, produce user input.
%% Mutually recursive with user_input.
-spec get_input_pos(mark | open, string()) -> minesweeper:input().
get_input_pos(Command, Data) ->
    case io_lib:fread("~d ~d", Data) of
        {ok, [Row, Col], _Rest} ->
            {Command, minesweeper:make_pos(Row, Col)};
        _ ->
            print_help(),
            get_input()
    end.


%% Display a help message to user.
%% Caution: side effects.
-spec print_help() -> ok.
print_help() ->
    io:format("Commands:~n"),
    io:format("  m 2 3   marks cell at row 2 column 3 as a mine~n"),
    io:format("  o 2 3   opens cell at row 2 column 3~n"),
    io:format("  q       quits the game~n").
