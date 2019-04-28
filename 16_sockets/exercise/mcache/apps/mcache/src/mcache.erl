-module(mcache).
-author("kalinin").

%% API
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_acceptor/2]).



start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, Port} = application:get_env(mcache, port),
  io:format("Starting server at port ~p~n", [Port]),
  {ok, NumAcc} = application:get_env(mcache, numacc),
  {ok, ListenSocket} = gen_tcp:listen(Port,
    [binary, {active, false}, {reuseaddr, true}, {packet, line}]),
  [spawn(?MODULE, start_acceptor, [ListenSocket, ID])|| ID <- lists:seq(1, NumAcc)],
  {ok, ListenSocket}.  % Аналог стейта для работы в цикле

start_acceptor(ListenSocket, Id ) ->
  io:format("Take something ~p ~p ~n", [self(), Id]),
  io:format("Waiting for client ~n"),
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  io:format("Got client  ~p~n", [AcceptSocket]),
  handle_connection(ListenSocket, Id, AcceptSocket).




handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.



handle_connection(ListenSocket, Id, AcceptSocket)->
  case gen_tcp:recv(AcceptSocket, 0) of
    {ok, RawMsg} ->
      io:format("~p:~p: Got msg from client:~p ~n", [self(), Id, RawMsg]),
      [Msg|_]  = binary:split(RawMsg, <<"\r\n">>),
      SendMessage = parse_msg(Msg),
      gen_tcp:send(AcceptSocket, <<SendMessage/binary, "\r\n">>),
      handle_connection(ListenSocket, Id, AcceptSocket);
    {error, closed} -> io:format("This Connection closed~n"),
      start_acceptor(ListenSocket, Id)
  end.

parse_msg(Msg)->
  [Command|ParseMsg]  = binary:split(Msg, <<" ">>),
%%  MsgKV = get_kv(ParseMsg),
  case Command of
    <<"GET">> -> storage({get, ParseMsg});
    <<"GETS">> ->storage({gets, ParseMsg});
    <<"SET">> -> storage({set, ParseMsg});
    <<"DELETE">> ->  storage({delete, ParseMsg});
    <<"ADD">> ->  storage({add, ParseMsg});
    <<"REPLACE">> -> storage({replace, ParseMsg});
    <<"APPEND">> -> storage({append, ParseMsg});
    <<"PREPEND">> -> storage({prepend, ParseMsg});
    _ ->  storage({unknown})
  end.

get_kv(Msg)->
  Msg1 = list_to_binary(Msg),
  binary:split(Msg1, <<" ">>).



storage({get, Key})->
  Key1 = list_to_binary(Key),
  case mcache_storage:get_mcache(Key1) of
      not_found -> <<"NOT FOUND">>;
      Value -> <<"VALUE ", Key1/binary, " ", Value/binary, "\r\nEND">>
  end;

storage({gets, ParseMsg1})->
  ParseMsg = list_to_binary(ParseMsg1),
  Raw_Keys = binary:split(ParseMsg, <<" ">>, [global]),
  Keys = lists:delete(<<>>, Raw_Keys),

  Raw_Msg = list_to_binary(lists:foldr(fun(Key, Acc) ->
    case mcache_storage:get_mcache(Key) of
      not_found -> [<<"VALUE ", Key/binary, " NOT FOUND", "\r\n">>|Acc];
      Value -> [<<"VALUE ", Key/binary, " ", Value/binary, "\r\n">>|Acc]
    end end, [], Keys)),
  <<Raw_Msg/binary, "END">>;

storage({delete, Key})->
  Key1 = list_to_binary(Key),
  case mcache_storage:delete_mcache(Key1) of
    not_found -> <<"NOT FOUND">>;
    true -> <<"DELETED">>
  end;

storage({set, ParseMsg})->
  Msg = list_to_binary(ParseMsg),
  [Key, Value] = binary:split(Msg, <<" ">>),
  mcache_storage:set_mcache({Key, Value});

storage({add, ParseMsg})->
  Msg = list_to_binary(ParseMsg),
  [Key, Value] = binary:split(Msg, <<" ">>),
  mcache_storage:add_mcache({Key, Value});

storage({replace, ParseMsg})->
  Msg = list_to_binary(ParseMsg),
  [Key, Value] = binary:split(Msg, <<" ">>),
  mcache_storage:replace_mcache({Key, Value});

storage({append, ParseMsg})->
  Msg = list_to_binary(ParseMsg),
  [Key, Value] = binary:split(Msg, <<" ">>),
  mcache_storage:append_mcache({Key, Value});

storage({prepend, ParseMsg})->
  Msg = list_to_binary(ParseMsg),
  [Key, Value] = binary:split(Msg, <<" ">>),
  mcache_storage:prepend_mcache({Key, Value});

storage({unknown})->
  <<"UNKNOWN REQUEST">>.
