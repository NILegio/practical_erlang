-module(worker).
-export([worker1/3, parse_file/1]).
%%-export([worker1/2, parse_file/1]).

%%worker1(ReducerPid, File)->
worker1(ReducerPid, Ref, File)->
  io:format("worker ~p~n", [self()]),
  Res = parse_file(File),
  ReducerPid ! {data, Ref, Res}, ok.
%%  ReducerPid ! {data, Res}, ok.

parse_file(File)->
  {ok, Content} = file:read_file(File),
  Str = binary:split(Content, [<<"\n">>], [global]),
  lists:foldl(fun (Word, Acc) -> Word1 = binary:split(Word, <<",">>, [global]),
    case list_to_tuple(Word1) of
      {_, Veg, AmountStr, _} -> {Amount, _} = string:to_integer(binary:bin_to_list(AmountStr)),
      Acc#{Veg => Amount};

      {<<>>} -> Acc end
    end, #{}, Str).
