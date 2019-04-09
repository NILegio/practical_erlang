-module(template).

-export([parse/2, parse_error1/2]).

parse(Str, Data) when is_binary(Str) ->
%%чтобы не было проблемы с парсингом ненужных слов, можно сначало преобразовывать ключи в словаре
%% в ключи с фигурными ковычками
    Temp = binary:split(Str, [<<"}}">>], [global]),
    F = fun(Word, Acc)->
      case binary:split(Word, [<<"{{">>]) of
        [Nothing] -> [Nothing|Acc];
        [Word1,Word2]-> case maps:find(Word2, Data) of
            {ok, Values}-> case is_integer(Values)of
                             true -> [Word1, integer_to_list(Values)|Acc];
                             false ->[Word1, Values|Acc]
                           end;
            error -> [Word1|Acc]
          end
        end
      end,
    Bin = lists:foldr(F, [], Temp),
    iolist_to_binary(Bin).



parse_error1(Str, Data) when is_binary(Str) ->
%%  не работает через фодл, так как бинари меняется
    Data1 = maps:fold(fun(K,V, Acc)->
    K1 = iolist_to_binary([<<"{{">>, K, <<"}}">>]),
      Acc#{ K1 => V } end, #{}, Data),
    maps:fold(fun(K,V, Bin)-> binary:replace(Bin, K, V) end, Str, Data1).
%%    binary:split(In, [<<"{{">>,<<"}}">>], [global]).

%Data = #{<<"name">> => "Kate",<<"wins">> => 55,<<"points">> => 777},