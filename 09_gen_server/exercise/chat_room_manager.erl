-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2,
          loop/1, stop/1]).
-record(room, {roomname, users = [], messages = []}).

start() ->
    Rooms = #{},
    spawn(?MODULE, loop, [Rooms]).


create_room(Server, RoomName) ->
    call(Server, {create_room, RoomName}).


remove_room(Server, RoomId) ->
    call (Server, {remove_room, RoomId}).


get_rooms(Server) ->
    call(Server, {get_rooms}).


add_user(Server, RoomId, UserName) ->
    call (Server, {add_user, RoomId, UserName}).


remove_user(Server, RoomId, UserName) ->
    call (Server, {remove_user, RoomId, UserName}).


get_users_list(Server, RoomId) ->
  call (Server, {get_users_list, RoomId}).


send_message(Server, RoomId, UserName, Message) ->
  call (Server, {send_message, RoomId, UserName, Message}).


get_messages_history(Server, RoomId) ->
  call (Server, {get_messages_history, RoomId}).


stop(Server) -> Server ! stop.

%%call_get(ServerPid, Staff)->
%%  call (ServerPid, {get_room, Staff}).

call(ServerPid, Staff)->
  QuickRef = make_ref(),
  ServerPid ! {self(), QuickRef, Staff},
  receive
    {reply, QuickRef, Reply} ->
      erlang:demonitor(QuickRef, [flush]),
      Reply
  after
    5000 -> erlang:demonitor(QuickRef, [flush]), {error, not_reply}
  end.




loop(Rooms)->
  io:format("Version 0.04, loop ~p, Rooms:~p, size Rooms ~p ~n", [self(), Rooms, map_size(Rooms)]),
  receive
    {ClientPid, QuickRef, Staff} ->
      {Reply,Rooms2} = handle_call(Staff, Rooms),
      ClientPid ! {reply, QuickRef, Reply},
      ?MODULE:loop(Rooms2);
    stop -> %io:format("It's over"), почему выдает ошибку
      ok;
    Msg -> io:format("Unknown message ~p for ~p ~n", [Msg, self()]), ?MODULE:loop(Rooms)
  end.
%%
%%handle_call({get_room, TempStaff}, Rooms)->
%%  {Command, Staff}= TempStaff,
%%  case maps:find(RoomId, Rooms) of
%%    error -> {{error, room_not_found},Rooms};
%%    {ok, _} -> Rooms2 = maps:remove(RoomId, Rooms), {ok, Rooms2}
%%  end;


handle_call({create_room, RoomName}, Rooms) ->
  case maps:size(Rooms) of
    L when L >= 5 -> {{error, room_limit}, Rooms}; %почему так? ведь должно быть L>5
    _ -> RoomID = rand:uniform(1000),
      Rooms2 = Rooms#{RoomID => #room{roomname = RoomName}},%Rooms2 = Rooms#room{roomname = RoomName, roomid =RoomID},
      {{ok, RoomID}, Rooms2}
  end;
handle_call({remove_room, RoomId}, Rooms)->

  case maps:find(RoomId, Rooms) of
    error -> {{error, room_not_found},Rooms};
    {ok, _} -> Rooms2 = maps:remove(RoomId, Rooms), {ok, Rooms2}
  end;
handle_call({get_rooms}, Rooms) ->
  Maps = maps:fold(fun(K, #room{roomname = V}, Acc)-> maps:put(K, V, Acc) end, maps:new(), Rooms),
  {maps:to_list(Maps), Rooms};
handle_call({add_user, RoomId, UserName}, Rooms)->
  % подумать, что делать если забыли указать Username в запросе
  case maps:find(RoomId, Rooms) of
    error -> {{error, room_not_found},Rooms};
    {ok, Values} -> Users = Values#room.users,
      case lists:member(UserName, Users) of
          true ->  {{error, user_is_in_room}, Rooms};
          false -> Rooms2 = Rooms#{RoomId => Values#room{users = [UserName|Users]}},
            {ok, Rooms2}
      end
  end;
handle_call({remove_user, RoomId, UserName}, Rooms)->

  case maps:find(RoomId, Rooms) of
    error -> {{error, room_not_found},Rooms};
    {ok, Values} -> Users = Values#room.users,
              case lists:member(UserName, Users) of
                false ->  {{error, user_not_in_room}, Rooms};
                true -> Rooms2 = Rooms#{RoomId => Values#room{users = lists:delete(UserName,Users)}},
                {ok, Rooms2}
              end
    end;
handle_call({get_users_list, RoomId}, Rooms)->
  case maps:find(RoomId, Rooms) of
    error -> {{error, room_not_found},Rooms};
    {ok, Values} -> {{ok, Values#room.users}, Rooms}
  end;

handle_call({send_message, RoomId, UserName, Message}, Rooms)->
  case maps:find(RoomId, Rooms) of
    error -> {{error, room_not_found},Rooms};
    {ok, Values} -> {Users, Messages} = {Values#room.users, Values#room.messages},
      case lists:member(UserName, Users) of
        false ->  {{error, user_not_in_room}, Rooms};
        true ->
          [User] = lists:filter(fun(User)-> case User of
                                            UserName -> true;
                                            _ -> false
                                          end end, Users),
          Rooms2 = Rooms#{RoomId => Values#room{messages = [{User, Message}|Messages]}},
          {ok, Rooms2}
      end
  end;

handle_call({get_messages_history, RoomId}, Rooms)->
  case maps:find(RoomId, Rooms) of
    error -> {{error, room_not_found},Rooms};
    {ok, Values} -> Messages = Values#room.messages, {{ok, Messages}, Rooms}
  end.


