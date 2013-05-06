-module(test_client_sup).
-behaviour(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0, init/1]).
-export([start_client/2, stop_client/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    Res = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    case application:get_env(test_client, default_connections) of
        undefined  -> ok;
        {ok, List} -> [ start_client(Name, Options) || {Name, Options} <- List ]
    end,
    Res.

start_client(Name, Options) ->
    Res = supervisor:start_child(?MODULE, [Name, Options]),
    lager:debug("Spawning cliend ~p: ~p", [Name, Res]),
    Res.

stop_client(Name) ->
    case global:whereis_name({test_client_conn, Name}) of
        undefined -> {error, {no_such_connection, Name}};
        ConnPid when is_pid(ConnPid) -> supervisor:terminate_child(?MODULE, ConnPid)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, { {simple_one_for_one, 0, 1}, [
        {test_client_conn, {test_client_conn, start_link, []},
            temporary, 60000, worker, [test_client_conn]}
    ]} }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
