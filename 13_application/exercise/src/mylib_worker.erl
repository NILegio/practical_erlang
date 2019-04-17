-module(mylib_worker).
-behavior(gen_server).

-export([start_link/0, get_version/0, get_modules/0, get_min_val/0, get_connection_timeout/0, all_apps/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {user_id, user_name}).

start_link()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



get_version()->
  {ok, Version} = application:get_key(mylib, vsn),
  Version.

get_modules()->
  {ok, Modules} = application:get_key(mylib, modules),
  Modules.

get_min_val()->
  {ok, Min_val} = application:get_env(mylib, min_val),
  Min_val.

get_connection_timeout()->
  {ok, Timeout} = application:get_env(mylib, connection_timeout),
  Timeout.

all_apps()->
  List = application:which_applications(),
  lists:foldl(fun({Name, Description, Version}, Acc)->
    Acc#{Name => #{description => Description, version => Version}} end, #{}, List).


init([]) ->
  {ok, #state{user_id = [], user_name = []}}.

%%handle_call({all_apps}, _Form, State) -> {reply, ok, State};

handle_call(_Any, _Form, State) -> {reply, ok, State}.

handle_cast(_Any, State) -> {no_reply, State}.

handle_info(_Request, State)-> {no_reply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.