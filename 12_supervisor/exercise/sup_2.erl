-module(sup_2).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).

%% TODO
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  SupervisorSpecification = #{
    strategy => one_for_one,
    intensity => 10,
    period => 60},

  ChildSpecifications =
    [#{id => worker_3,
      start => {worker, start_link, [rand:uniform(50)]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [worker]},
      #{id => worker_4,
        start => {worker, start_link, [rand:uniform(50)]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [worker]}
    ],
  {ok, {SupervisorSpecification, ChildSpecifications}}.

add_worker(Pass) ->
%%  Id_Iolist = ["worker ", rand:uniform(100)],
%%  Id_binary = iolist_to_binary(Id_Iolist),
%%  Id_atom = binary_to_atom(Id_binary, latin1),
  supervisor:start_child(
  ?MODULE,
  {Pass,
    {worker, start_link, [Pass]},
    permanent,
    2000,
    worker,
    [worker]}).

remove_worker(Pass) ->
  supervisor:terminate_child(?MODULE, Pass),
  supervisor:delete_child(?MODULE, Pass).