-module(url_parser).

-export([parse/1]).


-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(URL) ->
    URL1 = binary:split(URL, <<"://">>),
    get_main(URL1, #{}).

get_main(URL, Map)->
    case list_to_tuple(URL) of
        {_} -> {error, invalid_protocol};
        {_, <<>>} -> {error, invalid_domain};
        {Protocol, Url1} -> get_domain(binary:split(Url1, <<"/">>), Map#{protocol => Protocol})
    end.

 get_domain(Url, Map)->
        case list_to_tuple(Url) of
            {Domain} -> {ok, Map#{domain => Domain}};
            {Domain, Path} -> get_query(binary:split(Path, <<"?">>), Map#{domain => Domain})
    end.


get_query(Path, Map)->
    case list_to_tuple(Path) of
        {Path1} -> parse_path(Path1, Map#{query=><<>>});
        {Path1, Query} -> parse_path(Path1, Map#{query=>Query})
    end.

parse_path(Path, Map) ->
    Path1 = binary:split(Path, <<"/">>, [global]),
    case Path1 of
        [<<>>] -> {ok, Map#{path=>[], date => undefined}};
        _ -> Path2 = lists:delete(<<>>, Path1),
            get_date(Map#{path=>Path2})
    end.

get_date(#{path := Path}=Map) ->
    Raw_Date = lists:foldl(fun(Element, Acc) ->
    List = binary_to_list(Element),
    try
        Integer = erlang:list_to_integer(List), [Integer|Acc]
    catch
        error:_Error -> Acc
    end end, [], Path),
    Date = lists:reverse(Raw_Date),
    parse_date(Map, Date).

parse_date(Map, Date) when length(Date) < 3-> {ok, Map#{date => undefined}};
parse_date(Map, Date) ->
    {Year, Month, Day} = list_to_tuple(Date),
    if
        Month >=1, Month =< 12, Day >=1, Day =< 31 ->
            {ok, Map#{date => {Year, Month, Day}}};
        true -> {ok, Map#{date => undefined}}
    end.



