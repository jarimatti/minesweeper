%%%-------------------------------------------------------------------
%%% @author Jarimatti <jarimatti@iawe.local>
%%% @copyright (C) 2014, Jarimatti
%%% @doc
%%% Minesweeper gen_server game.
%%%
%%% Actually, the game could be gen_fsm instead.
%%% @end
%%% Created : 20 Oct 2014 by Jarimatti <jarimatti@iawe.local>
%%%-------------------------------------------------------------------
-module(ms_server).

-behaviour(gen_server).

%% API
-export([start_link/3,
         make_pos/2,
         open/2,
         flag/2,
         clear_flag/2,
         board_cell/2,
         get_board/1,
         quit/1]).

-export_type([row/0,
              col/0,
              mine_prob/0,
              pos/0,
              ui_cell/0,
              ui_board/0,
              ui_state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {board :: board(),
                visible :: visible(),
                flagged :: flagged()}).

-ifdef('TEST').
-include_lib("eunit/include/eunit.hrl").
-endif.


%%%===================================================================
%%% Data Definitions
%%%===================================================================

%%--------------------------------------------------------------------
%% API Data Definitions

%% UICell is one of:
%% - Integer [0..8]
%% - atom flag
%% - atom mine
%% - atom hidden
%% interp. an UI cell in the mine field:
%%         - Integer [0..8] is an empty cell with number neighboring mines
%%         - flag           is a flag cell
%%         - false_flag     is a flag on a cell without mine in endgame
%%         - mine           is a cell with mine in it
%%         - hidden         is an unrevealed cell
-type ui_cell() :: 0..8 | flag | false_flag | mine | hidden.

%% Row is Integer[1,30].
%% interp. the row coordinate.
-type row() :: 1..30.

%% Col is Integer[1,30].
%% interp. the column coordinate.
-type col() :: 1..30.

%% Pos is {Row, Col}.
%% interp. Position in the board.
-type pos() :: {row(), col()}.

%% Produce a new position from Row, Col coordinates.
-spec make_pos(row(), col()) -> pos().
make_pos(Row, Col) ->
    {Row, Col}.

%% GameState is one of:
%% - continue
%% - victory
%% - defeat
%% interp. - continue means that the game is not victory or defeat
%%         - victory means that all mines flag and all empty cells revealed
%%         - defeat means that a mine is revealed
-type game_state() :: continue | victory | defeat.

%% UIBoard is a {GameState Row, Col, [UICell]}.
%% interp. the board for UI, rows, cols and UI cells in row major order.
-type ui_board() :: {row(), col(), [ui_cell()]}.


%% Produce a new UI board from input data.
-spec make_ui_board(row(), col(), [ui_cell()]) -> ui_board().
make_ui_board(Rows, Cols, Data) when is_list(Data), Rows * Cols =:= length(Data) ->
    {Rows, Cols, Data}.


%% UIState is a {GameState, UIBoard}.
%% interp. the UI game state, including the board.
-type ui_state() :: {game_state(), ui_board()}.


%%--------------------------------------------------------------------
%% Private Data Definitions

%% Cell is one of:
%% - atom empty
%% - atom mine
%% interp. a cell in the mine field is either empty or has a mine.
-type cell() :: mine | empty.

%% Board is {Row, Col, [Cell]}.
%% where Row is rows in board
%%       Col is columns in board
%%       [Cell] is the list of cells.
%% interp. game board contents.
-type board() :: {row(), col(), [cell()]}.

%% MineProb is Integer[0,100].
%% interp. The propotion / probability of mines in the board, from 0% to 100%.
-type mine_prob() :: 0..100.

%% Visible is the set of visible cell positions.
-type visible() :: sets:set(pos()).

%% Produce an empty Visible positions.
-spec empty_visible() -> visible().
empty_visible() ->
    sets:new().

%% Flagged is the set of flagged cell positions.
-type flagged() :: sets:set(pos()).

%% Produce an empty Flagged positions.
-spec empty_flagged() -> flagged().
empty_flagged() ->
    sets:new().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(row(), col(), mine_prob()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Rows, Cols, MineProb) ->
    gen_server:start_link(?MODULE, {Rows, Cols, MineProb}, []).

%%--------------------------------------------------------------------
%% API functions.

%% Open an unrevealed, unflagged cell.
%% Returns the last game state if user keeps playing after game over.
-spec open(pid(), pos()) -> {ok, ui_state()}.
open(Pid, Pos) ->
    gen_server:call(Pid, {open, Pos}).

%% Mark an unrevealed cell as mine.
%% Returns the last game state if user keeps playing after game over.
-spec flag(pid(), pos()) -> {ok, ui_state()}.
flag(Pid, Pos) ->
    gen_server:call(Pid, {flag, Pos}).

%% Unmark an unrevealed cell as a mine.
-spec clear_flag(pid(), pos()) -> {ok, ui_state()}.
clear_flag(Pid, Pos) ->
    gen_server:call(Pid, {clear_flag, Pos}).

%% Produce the UI board.
-spec get_board(pid()) -> {ok, ui_state()}.
get_board(Pid) ->
    gen_server:call(Pid, get_board).

%% Stop the game.
-spec quit(pid()) -> ok.
quit(Pid) ->
    gen_server:cast(Pid, quit).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init({row(), col(), mine_prob()}) -> {ok, #state{}}.
init({Rows, Cols, MineProb}) ->
    random:seed(now()),
    {ok, #state{board = create_board(Rows, Cols, MineProb),
                visible = empty_visible(),
                flagged = empty_flagged()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({open, Pos}, _From, State) ->
    make_reply(open_pos(State, Pos));
handle_call({flag, Pos}, _From, State) ->
    make_reply(mark_pos(State, Pos));
handle_call({clear_flag, Pos}, _From, State) ->
    make_reply(unmark_pos(State, Pos));
handle_call(get_board, _From, State) ->
    {reply, {ok, ui_state(State)}, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(quit, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Produce a gen_server reply from game state.
-spec make_reply(#state{}) -> {reply, {ok, ui_state()}, #state{}} |
                              {stop, victory, {ok, ui_state()}, #state{}} |
                              {stop, defeat, {ok, ui_state()}, #state{}}.
make_reply(New_State) ->
    Reply = {ok, {Game_State, _UI_Board}} = {ok, ui_state(New_State)},
    case Game_State of
        victory ->
            {stop, normal, Reply, New_State};
        defeat ->
            {stop, normal, Reply, New_State};
        continue ->
            {reply, Reply, New_State}
    end.


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
    case random:uniform(100) < P of
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


%% Open an unflagged position in the board.
-spec open_pos(#state{}, pos()) -> #state{}.
open_pos(State = #state{board = Board, visible = Visible, flagged = Flagged}, Pos) ->
    case is_flagged(Flagged, Pos) or is_visible(Visible, Pos) of
        true ->
            State;
        false ->
            State#state{visible = compute_visible(Board, Visible, Flagged, Pos)}
    end.


%% Mark a hidden position in the board as suspected mine.
-spec mark_pos(#state{}, pos()) -> #state{}.
mark_pos(State = #state{visible = Visible, flagged = Flagged}, Pos) ->
    case is_visible(Visible, Pos) of
        true ->
            State;
        false ->
            State#state{flagged = add_flagged(Flagged, Pos)}
    end.

-ifdef('TEST').

mark_pos_test_() ->
    Empty = empty_flagged(),
    P1 = make_pos(1, 1),
    M1 = add_flagged(Empty, P1),
    P2 = make_pos(2, 2),
    EmptyV = empty_visible(),
    V2 = add_visible(EmptyV, P2),
    [?_assertMatch(#state{flagged = M1}, mark_pos(#state{visible = EmptyV, flagged = Empty}, P1)),
     ?_assertMatch(#state{flagged = Empty}, mark_pos(#state{visible = V2, flagged = Empty}, P2)),
     ?_assertMatch(#state{flagged = M1}, mark_pos(#state{visible = V2, flagged = Empty}, P1))].

-endif.

%% Unmark a flagged position in the board.
-spec unmark_pos(#state{}, pos()) -> #state{}.
unmark_pos(State = #state{flagged = Flagged}, Pos) ->
    State#state{flagged = del_flagged(Flagged, Pos)}.

-ifdef('TEST').

unmark_pos_test() ->
    Empty = empty_flagged(),
    P1 = make_pos(1, 1),
    M1 = add_flagged(Empty, P1),
    ?assertMatch(#state{flagged = Empty}, unmark_pos(#state{flagged = M1}, P1)).

-endif.

%% Add a Pos to Visible positions.
-spec add_visible(visible(), pos()) -> visible().
add_visible(Visible, Pos) ->
    sets:add_element(Pos, Visible).

-ifdef('TEST').

add_visible_test_() ->
    P1 = make_pos(1,1),
    Empty = empty_visible(),
    M1 = sets:add_element(P1, Empty),
    [?_assertEqual(M1, add_visible(Empty, P1)),
     ?_assertEqual(M1, add_visible(M1, P1))].

-endif.


%% Add a Pos to Flagged positions.
-spec add_flagged(flagged(), pos()) -> flagged().
add_flagged(Flagged, Pos) ->
    sets:add_element(Pos, Flagged).

-ifdef('TEST').

add_flagged_test_() ->
    P1 = make_pos(1,1),
    Empty_Flagged = empty_flagged(),
    M1 = sets:add_element(P1, Empty_Flagged),
    [?_assertEqual(M1, add_flagged(Empty_Flagged, P1)),
     ?_assertEqual(M1, add_flagged(M1, P1))].

-endif.

%% Remove a Pos from Flagged positions.
-spec del_flagged(flagged(), pos()) -> flagged().
del_flagged(Flagged, Pos) ->
    sets:del_element(Pos, Flagged).

-ifdef('TEST').

del_flagged_test_() ->
    P1 = make_pos(1,1),
    Empty = empty_flagged(),
    M1 = sets:add_element(P1, Empty),
    [?_assertEqual(Empty, del_flagged(Empty, P1)),
     ?_assertEqual(Empty, del_flagged(M1, P1))].

-endif.

%% Compute the UI game state from gen_server state.
-spec game_state(#state{}) -> game_state().
game_state(#state{board = Board, visible = Visible, flagged = Flagged}) ->
    Is_Mine_Visible = is_mine_visible(Board, Visible),
    All_Empty_Visible = board_all_type_in_set(Board, Visible, empty),
    All_Mines_Flagged = board_all_type_in_set(Board, Flagged, mine),
    case {Is_Mine_Visible, All_Empty_Visible, All_Mines_Flagged} of
        {true, _, _} ->
            defeat;
        {false, true, true} ->
            victory;
        {false, _, _} ->
            continue
    end.


%% Produce true if a mine is visible, false otherwise.
-spec is_mine_visible(board(), visible()) -> true | false.
is_mine_visible(Board, Visible) ->
    lists:any(fun (mine) ->
                      true;
                  (empty) ->
                      false
              end,
              lists:map(fun (Pos) ->
                                board_cell(Board, Pos)
                        end,
                        sets:to_list(Visible))).


%% Produce true if Visible has all empty cells in Board.
-spec board_all_type_in_set(board(), sets:set(pos()), cell()) -> true | false.
board_all_type_in_set(Board, Visible, Cell) ->
    Empty_Positions = board_positions_equal(Board, Cell),
    sets_are_equal(Empty_Positions, Visible).

-ifdef('TEST').

board_all_type_in_set_test_() ->
    Board = {3, 3, [empty, empty, mine,
                    empty, empty, mine,
                    mine, mine, empty]},
    Win = sets:from_list([make_pos(1,1), make_pos(1,2),
                          make_pos(2,1), make_pos(2,2),
                          make_pos(3,3)]),
    No_Win = sets:from_list([make_pos(1,1), make_pos(1,2),
                             make_pos(2,1), make_pos(2,2)]),
    [?_assertEqual(true, board_all_type_in_set(Board, Win, empty)),
     ?_assertEqual(false, board_all_type_in_set(Board, No_Win, empty))].

-endif.


%% Make an UI state from the gen_server state.
-spec ui_state(#state{}) -> ui_state().
ui_state(State) ->
    GameState = game_state(State),
    {GameState, ui_board(State, GameState)}.


%% Make an UI board from the gen_server state.
-spec ui_board(#state{}, game_state()) -> ui_board().
ui_board(#state{board = Board, visible = Visible, flagged = Flagged}, GameState) ->
    Data = lists:map(fun (Pos) ->
                             cell_to_ui_cell(Board,
                                             Pos,
                                             is_visible(Visible, Pos),
                                             is_flagged(Flagged, Pos),
                                             GameState)
                     end,
                     board_all_positions(Board)),
    make_ui_board(board_rows(Board), board_cols(Board), Data).


%% Map a Cell to UICell.
-spec cell_to_ui_cell(board(), pos(), boolean(), boolean(), game_state()) -> ui_cell().
cell_to_ui_cell(_Board, _Pos, false, false, continue) ->
    hidden;
cell_to_ui_cell(Board, Pos, false, false, _) ->
    case board_cell(Board, Pos) of
        mine ->
            mine;
        empty ->
            hidden
    end;
cell_to_ui_cell(_Board, _Pos, false, true, continue) ->
    flag;
cell_to_ui_cell(Board, Pos, false, true, _) ->
    case board_cell(Board, Pos) of
        mine ->
            flag;
        empty ->
            false_flag
    end;
cell_to_ui_cell(Board, Pos, true, false, _) ->
    case board_cell(Board, Pos) of
        mine ->
            mine;
        empty ->
            count_mines(board_neighbours(Board, Pos))
    end.


%% Produce true if the Pos is in the Visible positions, false otherwise.
-spec is_visible(visible(), pos()) -> true | false.
is_visible(Visible, Pos) ->
    sets:is_element(Pos, Visible).

-ifdef('TEST').

is_visible_test_() ->
    Empty = empty_visible(),
    P1 = make_pos(1, 1),
    M1 = add_visible(Empty, P1),
    [?_assertEqual(false, is_visible(Empty, P1)),
     ?_assertEqual(true, is_visible(M1, P1))].

-endif.


%% Produce true if the Pos is in the Flagged positions, false otherwise.
-spec is_flagged(flagged(), pos()) -> true | false.
is_flagged(Flagged, Pos) ->
    sets:is_element(Pos, Flagged).


-ifdef('TEST').

is_flagged_test_() ->
    Empty = empty_flagged(),
    P1 = make_pos(1, 1),
    M1 = add_flagged(Empty, P1),
    [?_assertEqual(false, is_flagged(Empty, P1)),
     ?_assertEqual(true, is_flagged(M1, P1))].

-endif.

%% Produce the number of rows in the board.
-spec board_rows(board()) -> row().
board_rows({Rows, _Cols, _Data}) ->
    Rows.


%% Produce the number of columns in the board.
-spec board_cols(board()) -> row().
board_cols({_Rows, Cols, _Data}) ->
    Cols.


%% Produce the data of the board in row-major order.
-spec board_data(board()) -> [cell()].
board_data({_Rows, _Cols, Data}) ->
    Data.

-ifdef('TEST').

board_data_test() ->
    Data = [mine, empty, mine,
            empty, mine, mine,
            mine, empty, mine,
            mine, mine, empty,
            mine, mine, empty],
    Board = {5, 3, Data},
    ?assertEqual(Data, board_data(Board)).

-endif.

%% Produce a cell in the position Row, Col from the Board.
-spec board_cell(board(), pos()) -> cell().
board_cell(Board, Pos) ->
    Index = pos_to_index(Board, Pos),
    lists:nth(Index, board_data(Board)).

-ifdef('TEST').

board_cell_test_() ->
    [?_assertEqual(empty, board_cell({1, 1, [empty]}, {1, 1})),
     ?_assertEqual(mine, board_cell({1, 2, [mine, empty]}, {1, 1})),
     ?_assertEqual(empty, board_cell({2, 2, [mine, empty, mine, mine]}, {1, 2})),
     ?_assertEqual(empty, board_cell({2, 2, [mine, mine, empty, mine]}, {2, 1})),
     ?_assertEqual(empty, board_cell({2, 2, [mine, mine, mine, empty]}, {2, 2}))].

-endif.


%% Produce a list of cells from board given a list of positions.
-spec board_cells(board(), [pos()]) -> [cell()].
board_cells(Board, Positions) ->
    lists:map(fun (Pos) -> board_cell(Board, Pos) end,
              Positions).

-ifdef('TEST').

board_cells_test() ->
    Board = {3, 3, [empty, mine, empty,
                    mine, mine, empty,
                    empty, empty, empty]},
    Positions = [make_pos(1,1), make_pos(1,2), make_pos(2,2)],
    ?assertEqual([empty, mine, mine], board_cells(Board, Positions)).

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


%% Produce the neighbouring cells of a Pos.
%% Ordering is implementation defined.
-spec board_neighbours(board(), pos()) -> [cell()].
board_neighbours(Board, Pos) ->
    board_cells(Board, neighbouring_positions(Board, Pos)).

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


%% Produce a set of coordinates that are visible from the Row, Col.
%% 
%% A cell is visible from an empty cell if it's empty and nort, south,
%% west or east neighbour. Recursion occurs if the cell has no mines
%% in any neighbouring cells and is not flagged.
-spec compute_visible(board(), visible(), flagged(), pos()) -> visible().
compute_visible(Board, Visible, Flagged, Pos) ->
    Seen = is_visible(Visible, Pos),
    Mark = is_flagged(Flagged, Pos),
    Cell = board_cell(Board, Pos),
    Neighbouring_Mines = count_mines(board_neighbours(Board, Pos)),
    case {Mark, Seen, Cell, Neighbouring_Mines} of
        {_, _, mine, _} ->
            add_visible(Visible, Pos);
        {_, true, _, _} ->
            Visible;
        {true, _, _, _} ->
            Visible;
        {false, false, empty, X} when X > 0 ->
            add_visible(Visible, Pos);
        {false, false, empty, 0} ->
            lists:foldl(fun (Pos_N, Acc) ->
                                compute_visible(Board, Acc, Flagged, Pos_N)
                        end,
                        add_visible(Visible, Pos),
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
    %% TODO: Should a mark prevent NSEW empty cells with neighboring
    %% mines to be visible? In this case it would be the make_pos(2,2)
    %% that would be not revealed.
    Visible_flagged = sets:from_list([make_pos(1,1), make_pos(2,1), make_pos(2,2)]),
    Flagged = sets:from_list([make_pos(1,2)]),
    [?_assertEqual(true,
                   sets_are_equal(Visible_1,
                                  compute_visible(Board,
                                                  empty_visible(),
                                                  empty_flagged(),
                                                  make_pos(1,1)))),
     ?_assertEqual(true,
                   sets_are_equal(Visible_2,
                                  compute_visible(Board,
                                                  empty_visible(),
                                                  empty_flagged(),
                                                  make_pos(2,2)))),
     ?_assertEqual(true,
                   sets_are_equal(Visible_3,
                                  compute_visible(Board,
                                                  empty_visible(),
                                                  empty_flagged(),
                                                  make_pos(3,3)))),
     ?_assertEqual(true,
                   sets_are_equal(Visible_flagged,
                                  compute_visible(Board,
                                                  empty_visible(),
                                                  Flagged,
                                                  make_pos(1,1))))].

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
-spec board_positions_equal(board(), cell()) -> sets:set(pos()).
board_positions_equal(Board, Cell) ->
    lists:foldl(fun (Pos, Acc) ->
                        case board_cell(Board, Pos) of
                            Cell ->
                                sets:add_element(Pos, Acc);
                            _ ->
                                Acc
                        end
                end,
                sets:new(),
                board_all_positions(Board)).

-ifdef('TEST').

board_positions_equal_test_() ->
    Board = {3, 3, [empty, empty, mine,
                    empty, empty, mine,
                    mine, mine, empty]},
    Empty_Positions_Set = sets:from_list([make_pos(1,1), make_pos(1,2),
                                          make_pos(2,1), make_pos(2,2),
                                          make_pos(3,3)]),
    Mine_Positions_Set = sets:from_list([make_pos(1,3), make_pos(2,3),
                                         make_pos(3,1), make_pos(3,2)]),
    [?_assertEqual(true, sets_are_equal(Empty_Positions_Set,
                                        board_positions_equal(Board, empty))),
     ?_assertEqual(true, sets_are_equal(Mine_Positions_Set,
                                        board_positions_equal(Board, mine)))].

-endif.


%% Produce a list of all positions in the Board in row-major order.
-spec board_all_positions(board()) -> [pos()].
board_all_positions(Board) ->
    lists:flatmap(fun (Row) ->
                          lists:map(fun (Col) ->
                                            make_pos(Row, Col)
                                    end,
                                    lists:seq(1, board_cols(Board)))
                  end,
                  lists:seq(1, board_rows(Board))).

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
