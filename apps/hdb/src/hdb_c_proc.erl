-module(hdb_c_proc).
-behavior(gen_server).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,code_change/3
        ,terminate/2]).
-export([start_link/1]).

start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, #{}, []).

init(State) ->
    lager:info("state=~p", [State]),
    {ok, Pid, I} = exec:run_link("sleep 10", [{stdout, fun child_log/3}]),
    {ok, State#{pid => {Pid, I}}}.

handle_call(_,_,State) ->
    {reply, ok, State}.

handle_cast(_,State) ->
    {noreply, State}.

handle_info(_,State) ->
    {noreply, State}.

code_change(_,_,State) ->
    {ok, State}.

terminate(_,_) ->
    ok.

child_log(Stream, CPid, Data) ->
    lager:info("~w:~w ~p",[CPid, Stream, Data]).
