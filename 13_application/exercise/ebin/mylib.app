{application, mylib,
  [{description, "app for homework 13"},
    {vsn, "0.1"},
    {registered, [mylib_sup, mylib_worker]},
    {mod, {mylib_app, []}},
    {applications,[kernel,stdlib]},
    {env,[
      {min_val, 2},
      {max_val, 10},
      {connection_timeout, 10000},
      {query_timeout, 10000}
      ]},
    {modules, [mylib_app, mylib_sup, mylib_worker]}
  ]}.
