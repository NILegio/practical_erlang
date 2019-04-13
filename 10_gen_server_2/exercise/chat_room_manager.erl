-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1,
        close_room/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



-record(state, {rooms}).


start_link()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%%module api


create_room(RoomName) ->
  gen_server:call(?MODULE, {create_room, RoomName}).

get_rooms()-> gen_server:call(?MODULE, {get_rooms}).

add_user(RoomPid, Username, UserPid) when is_binary(Username) ->
  get_room(RoomPid, {add_user, RoomPid, Username, UserPid}).
%%  gen_server:call(?MODULE, {add_user, RoomPid, Username, UserPid}).

remove_user(RoomPid, UserPid) ->
  get_room(RoomPid, {remove_user, RoomPid, UserPid}).
  %gen_server:call(?MODULE, {remove_user, RoomPid, UserPid}).

get_users(RoomPid)->
  get_room(RoomPid, {get_users, RoomPid}).
%%  gen_server:call(?MODULE, {get_users, RoomPid}).

send_message(RoomPid, Username, Msg)->
  get_room(RoomPid, {send_message, RoomPid, Username, Msg}).
%%  gen_server:call(?MODULE, {send_message, RoomPid, Username, Msg}).

get_history(RoomPid)->
  get_room(RoomPid, {get_history, RoomPid}).
%%  gen_server:call(?MODULE, {get_history, RoomPid}).

close_room(RoomPid) ->
  get_room (RoomPid, {close_room, RoomPid}).

get_room(RoomPid, Staff)->
  gen_server:call(?MODULE, {get_room, RoomPid, Staff}).


%%gen_server  api

init([])-> {ok, #state{rooms = #{}}}.

handle_call({create_room, RoomName}, _Form, State)->
  {ok, RoomPid} = chat_room:start_link(),
  Rooms = State#state.rooms,
  State2 = State#state{rooms = Rooms#{RoomPid => RoomName}},
  {reply, {RoomName, RoomPid}, State2};
handle_call({get_rooms}, _Form, State)->
  Res = in_get_users(State#state.rooms),
  {reply, Res, State};

handle_call({get_room, RoomPid, Staff}, _Form, State)->
  Rooms = State#state.rooms,
  case maps:find(RoomPid, Rooms)  of
    error -> {reply, {error, room_not_found},State};
    {ok, _Values} -> handle_call(Staff, _Form, State)
  end;

handle_call({add_user, RoomPid, Username, UserPid}, _Form, State)->
  {reply, chat_room:add_user(RoomPid, Username, UserPid),State};

handle_call({remove_user, RoomPid, UserPid}, _Form, State)->
  {reply, chat_room:remove_user(RoomPid, UserPid), State};
handle_call({get_users, RoomPid}, _Form, State)->
  List = chat_room:get_users(RoomPid),
  {reply, {ok, List}, State};
handle_call({send_message, RoomPid, Username, Msg}, _Form, State)->
  {reply, chat_room:add_message(RoomPid, Username, Msg), State};
handle_call({get_history, RoomPid}, _Form, State)->
  Messages = chat_room:get_history(RoomPid),
  {reply, {ok, Messages}, State};

handle_call({close_room, RoomPid}, _Form, State)->
  handle_cast({close_room, RoomPid}, State);




handle_call(_Any, _Form, State) -> {no_reply, State}.
handle_cast({close_room, RoomPid}, State) ->
    Rooms = State#state.rooms,
    State2 = State#state{rooms = maps:remove(RoomPid, Rooms)},
    {reply, ok, State2};

handle_cast(_Any, State) -> {no_reply, State}.

handle_info(_Request, State)-> {no_reply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.



%%inner function


in_get_users(State)->
  maps:fold(fun (K,V, Acc)-> [{V, K}|Acc] end, [],State).

