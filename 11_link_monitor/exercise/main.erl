-module(main).

-export([parse/1, loop/2, aggregate/2]).


parse(Files) ->
    io:format("reducer ~p ~n", [self()]),
    Wait =
        lists:map(fun (File)->
        WPid = spawn(worker, worker1, [self(), File]),
            MRef = erlang:monitor(process, WPid), {MRef, WPid, File} end, Files),
    io:format("Wait:~p~n", [Wait]),
    loop(Wait, {#{}, #{}}).

loop([],  Temp)-> Temp;
loop([{MRef, _, File}|Wait], {Acc, ErrorAcc})->
    receive
        {data, Res} -> loop(Wait, {aggregate(Res, Acc), ErrorAcc});
        {'DOWN', MRef, process, Pid, {TrueReason, _}} ->
            io:format("Worker ~p dont work~n", [Pid]),
            loop(Wait, {Acc, ErrorAcc#{File=>TrueReason}})
    after
        1000 -> {error, not_reply}
    end.


aggregate(Map1, Map2) ->
    maps:fold(
        fun(K,V, Acc)->
            case maps:find(K, Acc)  of
            {ok, Va}-> Acc#{K := V + Va};
            error -> Acc#{K => V}
        end end, Map1, Map2).

