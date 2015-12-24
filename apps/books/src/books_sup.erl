%%%-------------------------------------------------------------------
%% @doc books top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(books_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(_) ->
    SupFlags = #{strategy => one_for_all},
    Procs = [],
    {ok, { SupFlags, Procs} }.

%%====================================================================
%% Internal functions
%%====================================================================
