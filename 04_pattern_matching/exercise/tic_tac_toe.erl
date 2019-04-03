-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).


new_game() ->
    {{f, f, f},
     {f, f, f},
     {f, f, f}}.


win(GameState) ->win(GameState, [x, o, f]).
win(_GameState, [f|_])  -> no_win;
win(GameState, [E|T])->
  case GameState of
    {_, {E,E,E}, _} -> {win, E};
    {{_,E,_}, {_,E,_}, {_,E,_}} -> {win, E};
    {{E,_,_}, {_,E,_}, {_,_,E}}-> {win, E};
    {{_,_,E}, {_,E,_}, {E,_,_}}-> {win, E};
    {{E,E,E}, _,_} -> {win, E};
    {{E,_,_}, {E,_, _}, {E,_,_}} -> {win, E};
    {_, _,{E,E,E}} -> {win, E};
    {{_,_,E}, {_,_,E}, {_,_,E}} -> {win, E};

    _-> win(GameState, T) % добавить в другое тело
  end.





move(Cell, _Player, GameState) when Cell > length(GameState) ->
    {error, invalid_move};

move(Cell, Player, GameState) ->
  case {Cell, GameState} of
    {1, {{f, E1,E2},S1,S2}} -> {ok,  {{Player, E1, E2},S1,S2}};
    {2, {{E0, f, E2},S1,S2}} -> {ok, {{E0, Player, E2},S1,S2}};
    {3, {{E0, E1,f},S1,S2}} -> {ok,  {{E0, E1, Player},S1,S2}};
    {4, {S0,{f, E1,E2},S2}} -> {ok,  {S0,{Player, E1, E2},S2}};
    {5, {S0,{E0, f, E2},S2}} -> {ok, {S0,{E0, Player, E2},S2}};
    {6, {S0,{E0, E1,f},S2}} -> {ok,  {S0,{E0, E1, Player},S2}};
    {7, {S0,S1,{f, E1,E2}}} -> {ok,  {S0,S1,{Player, E1, E2}}};
    {8, {S0,S1,{E0, f, E2}}} -> {ok, {S0,S1,{E0, Player, E2}}};
    {9, {S0,S1,{E0, E1,f}}} -> {ok,  {S0,S1,{E0, E1, Player}}};
    _ -> {error, invalid_move}
  end.
