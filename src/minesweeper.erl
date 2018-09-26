%%% A minesweeper game.
-module(minesweeper).

-export([start_game/4,
         make_pos/2,
         get_cell/2,
         board_neighbours/2,
         count_mines/1]).

-export_type([board/0,
              cell/0,
              col/0,
              input/0,
              mine_prob/0,
              pos/0,
              result/0,
              row/0]).

-ifdef('TEST').
-include_lib("eunit/include/eunit.hrl").
-endif.


%%--------------------------------------------------------------------
%% Data Definitions
%%--------------------------------------------------------------------

%% Cell is one of:
%% - atom empty
%% - atom mine
%% interp. a cell in the mine field is either empty or has a mine.
-type cell() :: mine | empty.

%% Row is Integer[1,30].
%% interp. the row coordinate.
-type row() :: 1..30.

%% Col is Integer[1,30].
%% interp. the column coordinate.
-type col() :: 1..30.

%% Pos is {Row, Col}.
%% interp. Position in the board.
-type pos() :: {row(), col()}.

make_pos(Row, Col) ->
    {Row, Col}.

%% Board is {Row, Col, [Cell]}.
%% where Row is rows in board
%%       Col is columns in board
%%       [Cell] is the list of cells.
%% interp. game board contents.
-type board() :: {row(), col(), [cell()]}.

%% MineProb is Integer[0,100].
%% interp. The propotion / probability of mines in the board, from 0% to 100%.
-type mine_prob() :: 0..100.

%% Result is a tuple of
%% - atom victory or atom defeat
%% - board
%% interp. the state at the end of the game with the board.
-type result() :: {victory | defeat, board()}.

%% User input is one of:
%%  - {mark, Pos}
%%  - {open, Pos}
%%  - quit
%% interp. {mark, Pos} marks the position as a mine
%%         {open, Pos} opens the position
%%         quit quits the game
-type input() :: quit | {mark, pos()} | {open, pos()}.

%% IO is a Module.
%% interp. The IO module to use.
-type io() :: module().

%%--------------------------------------------------------------------
%% Public Functions (API)
%%--------------------------------------------------------------------

%% Produce the game result
-spec start_game(row(), col(), mine_prob(), io()) -> result().
start_game(N, M, P, IO) ->
    Board = create_board(N, M, P),
    play(Board, sets:new(), IO).

%%--------------------------------------------------------------------
%% Private Functions
%%--------------------------------------------------------------------

%% Play the game until quit, defeated or won.
%% Caution: side effects.
-spec play(board(), sets:set(pos()), io()) -> result().
play(Board, Visible, IO) ->
    IO:show_board(Board, Visible),
    case IO:get_input() of
        quit ->
            ok;
        {open, Pos} ->
            open_pos(Board, Pos, Visible, IO);
        {mark, Pos} ->
            mark_pos(Board, Pos, Visible, IO)
    end.

%% Open a position in the board, return game result.
%% Note: mutual recursion with play/2.
-spec open_pos(board(), pos(), sets:set(pos()), io()) -> result().
open_pos(Board, Pos, Visible, IO) ->
    case get_cell(Board, Pos) of
        mine ->
            IO:show_board(Board, sets:from_list(board_all_positions(Board))),
            {defeat, Board};
        empty ->
            Exposed = compute_visible(Board, Visible, Pos),
            case is_victory(Board, Exposed) of
                true ->
                    {victory, Board};
                false ->
                    play(Board, Exposed, IO)
            end
    end.

%% Mark a position in the board as mine, return game result.
%% Note mutual recursion with play/2.
-spec mark_pos(board(), pos(), sets:set(pos()), io()) -> result().
mark_pos(Board, _Pos, Visible, IO) ->
    %% stub
    play(Board, Visible, IO).

%% Produce the neighbouring cells of a Pos.
%% Ordering is implementation defined.
-spec board_neighbours(board(), pos()) -> [cell()].
board_neighbours(Board, Pos) ->
    get_cells(Board, neighbouring_positions(Board, Pos)).

-ifdef('TEST').

board_neighbours_test() ->
    %% The ordering is clockwise from north (top).
    Board = {5, 3, [mine, empty, mine,
                    empty, mine, mine,
                    mine, empty, mine,
                    mine, mine, empty,
                    mine, mine, empty]},
    ?assertEqual([empty, mine, empty],
                 board_neighbours(Board, make_pos(1, 1))).

-endif.

%% Produce a list of neighbouring Pos on the board from given Pos.
-spec neighbouring_positions(board(), pos()) -> [pos()].
neighbouring_positions(_Board = {_Rows, _Cols, _Data}, _Pos = {1, 1}) ->
    [make_pos(1,2), make_pos(2,2), make_pos(2,1)];
neighbouring_positions(_Board = {_Rows, Cols, _Data}, _Pos = {1, Cols}) ->
    [make_pos(2,Cols), make_pos(2,Cols-1), make_pos(1,Cols-1)];
neighbouring_positions(_Board = {Rows, _Cols, _Data}, _Pos = {Rows, 1}) ->
    [make_pos(Rows-1,1), make_pos(Rows-1,2), make_pos(Rows,2)];
neighbouring_positions(_Board = {Rows, Cols, _Data}, _Pos = {Rows, Cols}) ->
    [make_pos(Rows-1,Cols), make_pos(Rows,Cols-1), make_pos(Rows-1,Cols-1)];
%% sides
neighbouring_positions(_Board = {_Rows, _Cols, _Data}, _Pos = {1, Col}) ->
    [make_pos(1,Col+1), make_pos(2,Col+1), make_pos(2,Col),
     make_pos(2,Col-1), make_pos(1,Col-1)];
neighbouring_positions(_Board = {_Rows, _Cols, _Data}, _Pos = {Row, 1}) ->
    [make_pos(Row-1,1), make_pos(Row-1,2), make_pos(Row,2),
     make_pos(Row+1,2), make_pos(Row+1,1)];
neighbouring_positions(_Board = {Rows, _Cols, _Data}, _Pos = {Rows, Col}) ->
    [make_pos(Rows-1,Col), make_pos(Rows-1,Col+1), make_pos(Rows,Col+1),
     make_pos(Rows, Col-1), make_pos(Rows-1,Col-1)];
neighbouring_positions(_Board = {_Rows, Cols, _Data}, _Pos = {Row, Cols}) ->
    [make_pos(Row-1,Cols), make_pos(Row+1,Cols), make_pos(Row+1,Cols-1),
     make_pos(Row,Cols-1), make_pos(Row-1,Cols-1)];
%% middle
neighbouring_positions(_Board = {_Rows, _Cols, _Data}, _Pos = {Row, Col}) ->
    [make_pos(Row-1, Col), make_pos(Row-1, Col+1), make_pos(Row, Col+1),
     make_pos(Row+1, Col+1), make_pos(Row+1, Col), make_pos(Row+1, Col-1),
     make_pos(Row, Col-1), make_pos(Row-1, Col-1)].


-ifdef('TEST').

neighbouring_positions_test_() ->
    Board = {5, 3, [empty, empty, empty,
                    empty, empty, mine,
                    empty, mine, mine,
                    mine, empty, empty,
                    mine, mine, empty]},
    [%% corners
     ?_assertEqual([make_pos(1,2), make_pos(2,2), make_pos(2,1)],
                   neighbouring_positions(Board, make_pos(1,1))),
     ?_assertEqual([make_pos(2,3), make_pos(2,2), make_pos(1,2)],
                   neighbouring_positions(Board, make_pos(1,3))),
     ?_assertEqual([make_pos(4,1), make_pos(4,2), make_pos(5,2)],
                   neighbouring_positions(Board, make_pos(5,1))),
     ?_assertEqual([make_pos(4,3), make_pos(5,2), make_pos(4,2)],
                   neighbouring_positions(Board, make_pos(5,3))),
     %% sides
     ?_assertEqual([make_pos(1,1), make_pos(1,2), make_pos(2,2),
                    make_pos(3,2), make_pos(3,1)],
                   neighbouring_positions(Board, make_pos(2,1))),
     ?_assertEqual([make_pos(1,3), make_pos(3,3), make_pos(3,2),
                    make_pos(2,2), make_pos(1,2)],
                   neighbouring_positions(Board, make_pos(2,3))),
     ?_assertEqual([make_pos(1,3), make_pos(2,3), make_pos(2,2),
                    make_pos(2,1), make_pos(1,1)],
                   neighbouring_positions(Board, make_pos(1,2))),
     ?_assertEqual([make_pos(4,2), make_pos(4,3), make_pos(5,3),
                    make_pos(5,1), make_pos(4,1)],
                   neighbouring_positions(Board, make_pos(5,2))),
     %% middle
     ?_assertEqual([make_pos(1,2), make_pos(1,3), make_pos(2,3),
                    make_pos(3,3), make_pos(3,2), make_pos(3,1),
                    make_pos(2,1), make_pos(1,1)],
                   neighbouring_positions(Board, make_pos(2,2)))
    ].

-endif.

%% Produce a list of cells from board given a list of positions.
-spec get_cells(board(), [pos()]) -> [cell()].
get_cells(Board, Positions) ->
    lists:map(fun (Pos) -> get_cell(Board, Pos) end,
              Positions).

-ifdef('TEST').

get_cells_test() ->
    Board = {3, 3, [empty, mine, empty,
                    mine, mine, empty,
                    empty, empty, empty]},
    Positions = [make_pos(1,1), make_pos(1,2), make_pos(2,2)],
    ?assertEqual([empty, mine, mine], get_cells(Board, Positions)).

-endif.

%% Produce a count of mines in the list of cells.
-spec count_mines([cell()]) -> non_neg_integer().
count_mines(Cells) ->
    lists:foldl(fun (mine, Mines) ->
                        Mines + 1;
                    (empty, Mines) ->
                        Mines
                end,
                0,
                Cells).

-ifdef('TEST').

count_mines_test_() ->
    [?_assertEqual(0, count_mines([])),
     ?_assertEqual(0, count_mines([empty])),
     ?_assertEqual(1, count_mines([mine])),
     ?_assertEqual(1, count_mines([empty, mine])),
     ?_assertEqual(1, count_mines([mine, empty])),
     ?_assertEqual(2, count_mines([mine, mine]))].

-endif.

%% Produce a cell in the position Row, Col from the Board.
-spec get_cell(board(), pos()) -> cell().
get_cell(Board, Pos) ->
    Index = pos_to_index(Board, Pos),
    lists:nth(Index, board_data(Board)).

-ifdef('TEST').

get_cell_test_() ->
    [?_assertEqual(empty, get_cell({1, 1, [empty]}, {1, 1})),
     ?_assertEqual(mine, get_cell({1, 2, [mine, empty]}, {1, 1})),
     ?_assertEqual(empty, get_cell({2, 2, [mine, empty, mine, mine]}, {1, 2})),
     ?_assertEqual(empty, get_cell({2, 2, [mine, mine, empty, mine]}, {2, 1})),
     ?_assertEqual(empty, get_cell({2, 2, [mine, mine, mine, empty]}, {2, 2}))].

-endif.

%% Produce the data of the board as list (row-major).
-spec board_data(board()) -> [cell()].
board_data({_Rows, _Cols, Data}) ->
    Data.

-ifdef('TEST').

board_data_test() ->
    ?assertEqual([empty], board_data({1, 1, [empty]})).

-endif.

%% Produce an index into the board from position.
-spec pos_to_index(board(), pos()) -> non_neg_integer().
pos_to_index(_Board = {_Rows, Cols, _Data}, {Row, Col}) ->
    (Row - 1) * Cols + Col.

-ifdef('TEST').

pos_to_index_test_() ->
    Rows = 5,
    Cols = 3,
    Board = {Rows, Cols, []},
    [?_assertEqual(0 * Cols + 1, pos_to_index(Board, {1, 1})),
     ?_assertEqual(0 * Cols + 2, pos_to_index(Board, {1, 2})),
     ?_assertEqual(0 * Cols + 3, pos_to_index(Board, {1, 3})),
     ?_assertEqual(1 * Cols + 1, pos_to_index(Board, {2, 1})),
     ?_assertEqual(2 * Cols + 2, pos_to_index(Board, {3, 2}))].

-endif.

%% Produce a set of coordinates that are visible from the Row, Col.
%% 
%% A cell is visible from an empty cell if it's empty and nort, south,
%% west or east neighbour. Recursion occurs if the cell has no mines
%% in any neighbouring cells.
-spec compute_visible(board(), sets:set(pos()), pos()) -> sets:set(pos()).
compute_visible(Board, Visible, Pos) ->
    Seen = sets:is_element(Pos, Visible),
    Cell = get_cell(Board, Pos),
    Neighbouring_Mines = count_mines(board_neighbours(Board, Pos)),
    case {Seen, Cell, Neighbouring_Mines} of
        {_, mine, _} ->
            Visible;
        {true, _, _} ->
            Visible;
        {false, empty, X} when X > 0 ->
            sets:add_element(Pos, Visible);
        {false, empty, 0} ->
            lists:foldl(fun (Pos_N, Acc) ->
                                compute_visible(Board, Acc, Pos_N)
                        end,
                        sets:add_element(Pos, Visible),
                        neighbouring_positions(Board, Pos))
    end.

-ifdef('TEST').

compute_visible_test_() ->
    Board = {3, 3, [empty, empty, mine,
                    empty, empty, mine,
                    mine, mine, empty]},
    Visible_1 = sets:from_list([make_pos(1,1), make_pos(1,2),
                                make_pos(2,1), make_pos(2,2)]),
    Visible_2 = sets:from_list([make_pos(2,2)]),
    Visible_3 = sets:from_list([make_pos(3,3)]),
    [?_assertEqual(true,
                   sets_are_equal(Visible_1,
                                  compute_visible(Board, sets:new(),
                                                  make_pos(1,1)))),
     ?_assertEqual(true,
                   sets_are_equal(Visible_2,
                                  compute_visible(Board, sets:new(),
                                                  make_pos(2,2)))),
     ?_assertEqual(true,
                   sets_are_equal(Visible_3,
                                  compute_visible(Board, sets:new(),
                                                  make_pos(3,3))))].

-endif.

%% Produce true if Visible has all empty cells in Board.
-spec is_victory(board(), sets:set(pos())) -> true | false.
is_victory(Board, Visible) ->
    Empty_Positions = board_empty_positions(Board),
    sets_are_equal(Empty_Positions, Visible).

-ifdef('TEST').

is_victory_test_() ->
    Board = {3, 3, [empty, empty, mine,
                    empty, empty, mine,
                    mine, mine, empty]},
    Win = sets:from_list([make_pos(1,1), make_pos(1,2),
                          make_pos(2,1), make_pos(2,2),
                          make_pos(3,3)]),
    No_Win = sets:from_list([make_pos(1,1), make_pos(1,2),
                             make_pos(2,1), make_pos(2,2)]),
    [?_assertEqual(true, is_victory(Board, Win)),
     ?_assertEqual(false, is_victory(Board, No_Win))].

-endif.

%% Produce true if the sets are subsets of each other.
-spec sets_are_equal(sets:set(), sets:set()) -> true | false.
sets_are_equal(Set_A, Set_B) ->
    A = sets:is_subset(Set_A, Set_B),
    B = sets:is_subset(Set_B, Set_A),
    A and B.

-ifdef('TEST').

sets_are_equal_test_() ->
    Empty_A = sets:new(),
    Empty_B = sets:new(),
    A_1 = sets:from_list([1]),
    A_12 = sets:from_list([1, 2]),
    B_21 = sets:from_list([2, 1]),
    [?_assertEqual(true, sets_are_equal(Empty_A, Empty_B)),
     ?_assertEqual(false, sets_are_equal(Empty_A, A_1)),
     ?_assertEqual(false, sets_are_equal(A_1, Empty_A)),
     ?_assertEqual(false, sets_are_equal(A_1, A_12)),
     ?_assertEqual(true, sets_are_equal(A_12, B_21)),
     ?_assertEqual(true, sets_are_equal(B_21, A_12))].

-endif.

%% Produce a set of empty cells in Board.
-spec board_empty_positions(board()) -> sets:set(pos()).
board_empty_positions(Board) ->
    lists:foldl(fun (Pos, Acc) ->
                        case get_cell(Board, Pos) of
                            empty ->
                                sets:add_element(Pos, Acc);
                            mine ->
                                Acc
                        end
                end,
                sets:new(),
                board_all_positions(Board)).

-ifdef('TEST').

board_empty_positions_test() ->
    Board = {3, 3, [empty, empty, mine,
                    empty, empty, mine,
                    mine, mine, empty]},
    Empty_Positions_Set = sets:from_list([make_pos(1,1), make_pos(1,2),
                                          make_pos(2,1), make_pos(2,2),
                                          make_pos(3,3)]),
    ?assertEqual(true, sets_are_equal(Empty_Positions_Set,
                                      board_empty_positions(Board))).

-endif.

%% Produce a list of all positions in the Board.
-spec board_all_positions(board()) -> [pos()].
board_all_positions(_Board = {Rows, Cols, _Data}) ->
    lists:flatmap(fun (Row) ->
                          lists:map(fun (Col) ->
                                            make_pos(Row, Col)
                                    end,
                                    lists:seq(1, Cols))
                  end,
                  lists:seq(1, Rows)).

-ifdef('TEST').

board_all_positions_test() ->
    Board = {3, 3, [empty, empty, mine,
                    empty, empty, mine,
                    mine, mine, empty]},
    All = [make_pos(1,1), make_pos(1,2), make_pos(1,3),
           make_pos(2,1), make_pos(2,2), make_pos(2,3),
           make_pos(3,1), make_pos(3,2), make_pos(3,3)],
    ?assertEqual(All, board_all_positions(Board)).

-endif.

%% Produces a new N-by-M game board, where P% of the cells are mines.
-spec create_board(row(), col(), mine_prob()) -> board().
create_board(N, M, P) ->
    {N, M, [ initial_cell(P) || _ <- lists:seq(1, N*M) ]}.

-ifdef('TEST').

create_board_test_() ->
    [?_assertEqual({4, 3, [empty, empty, empty,
                           empty, empty, empty,
                           empty, empty, empty,
                           empty, empty, empty]},
                   create_board(4, 3, 0)),
     ?_assertEqual({3, 4, [mine, mine, mine, mine,
                           mine, mine, mine, mine,
                           mine, mine, mine, mine]},
                   create_board(3, 4, 100))].

-endif.

%% Produce mine on probability P, otherwise empty.
%% P is Integer[0, 100].
-spec initial_cell(mine_prob()) -> cell().
initial_cell(P) ->
    case rand:uniform(100) < P of
        true ->
            mine;
        false ->
            empty
    end.

-ifdef('TEST').

initial_cell_test_() ->
    [?_assertEqual(empty, initial_cell(0)),
     ?_assertEqual(mine, initial_cell(100))].

-endif.
