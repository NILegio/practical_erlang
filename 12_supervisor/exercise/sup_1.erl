-module(sup_1).

-export([start_link/0, init/1]).

%% TODO

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  SupervisorSpecification = #{
    strategy => one_for_one,
    intensity => 10,
    period => 60},

  ChildSpecifications =
    [#{id => worker_1,
      start => {worker, start_link, [rand:uniform(50)]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [worker]},
      #{id => worker_2,
        start => {worker, start_link, [rand:uniform(50)]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [worker]}
    ],
  {ok, {SupervisorSpecification, ChildSpecifications}}.

