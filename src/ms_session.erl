%%%-------------------------------------------------------------------
%%% @author Jarimatti <jarimatti@iawe.local>
%%% @copyright (C) 2016, Jarimatti
%%% @doc
%%%
%%% @end
%%% Created : 24 Feb 2016 by Jarimatti <jarimatti@iawe.local>
%%%-------------------------------------------------------------------
-module(ms_session).

-behaviour(gen_server).

%% API
-export([start_link/0, new/1, get/1, remove/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%==============================================================================
%% Data Definitions

-type id() :: integer().

-type state() :: dict:dict(integer(), pid()).


%%%===================================================================
%%% API
%%%===================================================================

%% Starts the server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% Produce new session with given Pid.
-spec new(pid()) -> {ok, id()}.
new(Game) ->
    gen_server:call(?SERVER, {new, Game}).


%% Produce the Pid associated with this session or undefined.
-spec get(id()) -> {ok, pid() | undefined}.
get(SessionID) ->
    gen_server:call(?SERVER, {get, SessionID}).


%% Remove the session.
-spec remove(id()) -> ok.
remove(SessionID) ->
    gen_server:cast(?SERVER, {remove, SessionID}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes the server
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, dict:new()}.

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
handle_call({new, Game}, _From, Sessions) ->
    SessionID = dict:size(Sessions) + 1,
    MonitorRef = monitor(process, Game),
    NewSessions = dict:store(SessionID,
                             #{pid => Game, ref => MonitorRef},
                             Sessions),
    Reply = {ok, SessionID},
    {reply, Reply, NewSessions};
handle_call({get, SessionID}, _From, Sessions) ->
    Game = case dict:find(SessionID, Sessions) of
               error ->
                   undefined;
               {ok, #{pid := Pid}} ->
                   Pid
           end,
    Reply = {ok, Game},
    {reply, Reply, Sessions}.


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
handle_cast({remove, SessionID}, Sessions) ->
    case dict:find(SessionID, Sessions) of
        error ->
            ok;
        {ok, #{ref := MRef}} ->
            demonitor(MRef)
    end,
    NewSessions = dict:erase(SessionID, Sessions),
    {noreply, NewSessions}.


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
handle_info({'DOWN', MonitorRef, process, Pid, _Info}, Sessions)
  when is_pid(Pid) ->
    SessionID = fetch_by_ref(MonitorRef, Sessions),
    NewSessions = dict:erase(SessionID, Sessions),
    {noreply, NewSessions};
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
terminate(_Reason, Sessions) ->
    dict:map(fun (_K, V = #{ref := MRef}) ->
                     demonitor(MRef),
                     V
             end,
             Sessions),
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


%% Produce the SessionID or not_found if MonitorRef is not found.
-spec fetch_by_ref(reference(), state()) -> id().
fetch_by_ref(MRef, Sessions) ->
    dict:fold(fun (K, #{ref := MRef2}, not_found) when MRef == MRef2 ->
                      K;
                  (_K, _V, A) ->
                      A
              end,
              not_found,
              Sessions).
