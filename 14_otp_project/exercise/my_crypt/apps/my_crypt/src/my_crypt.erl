%%%-------------------------------------------------------------------
%%% @author kalinin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Апр. 2019 11:11
%%%-------------------------------------------------------------------
-module(my_crypt).
-author("kalinin").

%% API
-behavior(gen_server).

-export([start_link/0, get_key/0, set_key/1, encode/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



%%-record(state, {
%%  key
%%}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_key()->
  gen_server:call(?MODULE, {get_key}).

set_key(New_key)
%%  when is_binary(New_key)
  ->
  gen_server:cast(?MODULE, {set_key, New_key}).

encode(Str)
%%  when is_binary(Str)
  ->
  gen_server:call(?MODULE, {encode, Str}).


%%% gen_server API

init([]) ->
  {ok, Key} = application:get_env(my_crypt, crypt_key),
  State = #{key => Key},
  {ok, State}.

handle_call({get_key}, _Form, State)->
  [Key] = maps:values(State),
  {reply, Key, State};

handle_call({encode, Str}, _Form, State)->
  Encode_Str = in_encode(Str, maps:get(key,State)),
  {reply, Encode_Str, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({set_key, New_key}, State) ->
  State2 = State#{key := New_key},
  {noreply, State2};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.


in_encode(Str, Key) ->
  Str1 = binary_to_list(Str),
  Key1 = binary_to_list(Key),
  in_encode(Str1, Key1, [], Key1).
in_encode([], _Key, Temp, _OKey) -> NewStr = lists:reverse(Temp), unicode:characters_to_binary(NewStr);
in_encode(Str, [], Temp, OKey) -> in_encode(Str, OKey, Temp, OKey);
in_encode([Char|Str], [Key_Char|Key], Temp, OKey)->
  NewChar = Char bxor Key_Char,
  in_encode(Str, Key, [NewChar|Temp], OKey).




