%%%-------------------------------------------------------------------
%% @doc hdb top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('hdb_sup').

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
init([]) ->
    SupFlags = #{strategy => one_for_one
                ,intensity => 100
                ,period => 5},
    CProc = #{id => cproc1
             ,start => {hdb_c_proc, start_link, [test]}
             ,type => worker},
    Procs = [CProc],
    {ok, { SupFlags, Procs} }.

%%====================================================================
%% Internal functions
%%====================================================================
