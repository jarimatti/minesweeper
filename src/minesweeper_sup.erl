-module(minesweeper_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {supervisor_flags(),
          [ms_server_sup(), ms_session()]}}.


%%====================================================================
%% Internal functions
%%====================================================================

supervisor_flags() ->
    #{strategy => one_for_all,
      intensity => 1,
      period => 5}.


ms_server_sup() ->
    #{id => ms_server_sup,
      start => {ms_server_sup, start_link, []},
      type => supervisor}.


ms_session() ->
    #{id => ms_session,
      start => {ms_session, start_link, []}}.
