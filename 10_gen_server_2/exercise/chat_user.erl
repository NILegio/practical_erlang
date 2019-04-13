-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {messages}).

start_link()-> gen_server:start_link(?MODULE, [], []).

%%module api

add_message(UserPid, Username, Msg)->
  gen_server:cast(UserPid, {add_message, Username, Msg}).

get_messages(UserPid)->
  gen_server:call(UserPid, {get_messages}).


%%gen_server  api

init([])->
  {ok, #state{messages=[]}}.


handle_call({get_messages}, _Form, State)->
  Res = State#state.messages,
  {reply, lists:reverse(Res), State};

handle_call(_Any, _Form, State) -> {no_reply, State}.

handle_cast({add_message, Username, Msg}, State)->
  Messages = State#state.messages,
  State2 = State#state{messages = [{Username, Msg}|Messages]},
  {noreply,  State2};

handle_cast(_Any, State) -> {no_reply, State}.

handle_info(_Request, State)-> {no_reply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%inner function

