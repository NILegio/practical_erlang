%%%-------------------------------------------------------------------
%%% @author kalinin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Апр. 2019 9:28
%%%-------------------------------------------------------------------
-module(mcache_storage).
-author("kalinin").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).
-export([set_mcache/1, get_mcache/1, delete_mcache/1, add_mcache/1, replace_mcache/1,
append_mcache/1, prepend_mcache/1]).

-define(SERVER, ?MODULE).

-record(state, {}).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set_mcache({Key, Value})->
  gen_server:call(?MODULE, {set, {Key, Value}}).

get_mcache(Key)->
  case ets:lookup(?MODULE, Key) of
    [{Key, Value}] -> Value;
    [] -> not_found
  end.

delete_mcache(Key) ->
  gen_server:call (?MODULE, {delete, Key}).

add_mcache({Key, Value}) ->
  case ?MODULE:get_mcache(Key) of
    not_found -> set_mcache({Key, Value});
    _ -> <<"EXISTS">>
  end.

replace_mcache({Key, Value}) ->
  case ?MODULE:get_mcache(Key) of
    not_found -> <<"NOT FOUND">>;
    _ -> set_mcache({Key, Value})
  end.

append_mcache({Key, New_Value}) ->
  case ?MODULE:get_mcache(Key) of
    not_found -> <<"NOT FOUND">>;
    Value -> set_mcache({Key, <<Value/binary, New_Value/binary>>})
  end.

prepend_mcache({Key, New_Value}) ->
  case ?MODULE:get_mcache(Key) of
    not_found -> <<"NOT FOUND">>;
    Value -> set_mcache({Key, <<New_Value/binary, Value/binary>>})
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([]) ->
  io:format("~p init ~n", [?MODULE]),
  ets:new(?MODULE, [named_table]),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------


handle_call({set, {Key, Value}}, _From, State) ->
  ets:insert(?MODULE, {Key, Value}),
  {reply, <<"STORED">>, State};

handle_call({delete, Key}, _From, State) ->
  case ets:lookup(?MODULE, Key) of
    [{Key, _Value}] -> {reply, ets:delete(?MODULE, Key), State};
    [] -> {reply, not_found, State}
  end;


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
