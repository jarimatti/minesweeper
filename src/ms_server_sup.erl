-module(ms_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor.
%% The restart type could be transient instead of temporary.
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Dynamically start a new ms_server child.
start_child(Width, Height, Mines) ->
    supervisor:start_child(?MODULE,
                           [Width, Height, Mines]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [?CHILD(ms_server, worker)]}}.
