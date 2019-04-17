-module(mylib_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).



start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
  SupervisorSpecification = #{
    strategy => one_for_one,
    intensity => 10,
    period => 60},

  ChildSpecifications =
    [#{id => mylib_worker,
      start => {mylib_worker, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [mylib_worker]}
    ],
  {ok, {SupervisorSpecification, ChildSpecifications}}.
