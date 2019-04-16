-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% TODO

-record(state, {id}).

start_link(ID)-> gen_server:start_link(?MODULE, {ID}, []).

%%module api

ping(Pid) ->
  gen_server:call(Pid, {ping, Pid}).


%%gen_server  api      [{_, Worker2_Pid, worker, _},{_, Worker1_Pid, worker, _}] = Sup1_Childs,

init({ID})->
  {ok, #state{id = ID}}.


handle_call({ping, Pid}, _Form, State)->
  Id = State#state.id,
  {reply, {Id, Pid}, State};

handle_call(_Any, _Form, State) -> {reply, ok, State}.

handle_cast(_Any, State) -> {no_reply, State}.

handle_info(_Request, State)-> {no_reply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.



