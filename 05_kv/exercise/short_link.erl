-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1]).

%%% module API

init() ->
    %% init randomizer
%%    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
%%    rand:seed(exsp, {A,B,C}),
%%    State = your_statement, State.
   maps:new().



create_short(LongLink, State) ->
    ValuesState = maps:values(State),
    case lists:member(LongLink, ValuesState) of
      true -> Raw_List = maps:to_list(State),
%%{_, {ShortLink,_}} = lists:search(fun({_,Link})-> Link =:= LongLink end, Raw_List), не работает функция
              [{ShortLink,_}] = lists:filter(fun({_,Link})-> Link =:= LongLink end, Raw_List),
              {ShortLink, State};
%%        {"aaaa", State};
      false -> ShortLink = "http://0110.og/"++rand_str(8),
               State1 = State#{ShortLink => LongLink},
               {ShortLink, State1}
    end.
%%    ShortLink = "http://0110.og/"++rand_str(8),
%%    State1 = State#{ShortLink => LongLink},
%%    {ShortLink, State1}.
%%    your_result.

get_long(ShortLink, State) ->
    Keys_ShortLink = maps:keys(State),
    case lists:member(ShortLink, Keys_ShortLink) of
      true -> maps:find(ShortLink, State);
      false -> {error, not_found}
    end.



%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [rand:uniform(110 - 48) + 47 || _ <- lists:seq(1, Length)]).
