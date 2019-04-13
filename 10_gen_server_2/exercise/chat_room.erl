-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {users, messages}).



start_link()-> gen_server:start_link(?MODULE, [], []).

%%module api

add_user(RoomPid, Username, UserPid) ->
  gen_server:cast(RoomPid, {add_user, Username, UserPid}).

remove_user(RoomPid, UserPid) ->
  gen_server:call(RoomPid, {remove_user, UserPid}).

get_users(RoomPid)->
  gen_server:call(RoomPid, {get_users}).

add_message(RoomPid, Username, Msg)->
  gen_server:cast(RoomPid, {add_message,  Username, Msg}).

get_history(RoomPid)->
  gen_server:call(RoomPid, {get_history}).

%%gen_server  api

init([])->
  {ok, #state{users = #{}, messages = []}}.


handle_call({get_users}, _Form, State)->
    Res = in_get_users(State#state.users),
    {reply, Res, State};

handle_call({remove_user, UserPid}, _Form, State)->
    {Reply, State2} = in_remove_user(State, UserPid),
    {reply, Reply, State2};

handle_call({get_history}, _Form, State)->
    Res = State#state.messages,
    {reply, lists:reverse(Res), State};

handle_call(_Any, _Form, State) -> {no_reply, State}.

handle_cast({add_user, Username, UserPid}, State)->
    Users = State#state.users,
    State2 = State#state{users = Users#{UserPid => Username}},
    {noreply, State2};


handle_cast({add_message, Username, Msg}, State)->
    State2 = in_add_message(State, Username, Msg),
    {noreply, State2};

handle_cast(_Any, State) -> {noreply, State}.

handle_info(_Request, State)-> {no_reply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%inner function

in_get_users(State)->
    maps:fold(fun (K,V, Acc)-> [{V, K}|Acc] end, [],State).

in_remove_user(State, UserPid)->
    Users = State#state.users,
    case maps:find(UserPid, Users) of
        error -> {{error, user_not_found}, State};
        {ok, _} -> State2 = State#state{users = maps:remove(UserPid, Users)}, {ok, State2}
    end.

in_add_message(State, Username, Msg)->
    {Messages, Users} = {State#state.messages, State#state.users},
    lists:foreach(fun(UserPid) -> chat_user:add_message(UserPid, Username, Msg) end, maps:keys(Users)),
    State#state{messages = [{Username, Msg}|Messages]}.

