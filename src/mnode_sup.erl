%%%-------------------------------------------------------------------
%% @doc mnode top level supervisor.5202;0c
%% @end
%%%-------------------------------------------------------------------

-module(mnode_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    WorkerChildren = build_workers(inet:getifaddrs()),
    Arbitrator = #{id => arbitrator, start => {arbitrator, start_link, []}},

    Children = [Arbitrator|WorkerChildren],

    {ok, { {one_for_all, 1, 1}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================

build_workers({ok, Addrs}) ->
    build_workers(Addrs, []).

build_workers([], Acc) ->
    Acc;

build_workers([{Name, Parts}|Tail], Acc) ->
    build_workers(Tail, build_worker(Name, Parts, []) ++ Acc).

%%====================================================================

build_worker(_Name, [], Acc) ->
    Acc;

build_worker(Name, [{flags, _}|Tail], Acc) ->
    build_worker(Name, Tail, Acc);

build_worker(Name, [{addr, Addr}, {netmask, _Netmask}|Tail], Acc) ->
    Child = #{id => build_name(Name, Addr),
	      start => {
		mnode_worker,
		start_link,
		[Name, Addr]}},
    build_worker(Name, Tail, [Child|Acc]);

build_worker(Name, [Head|Tail], Acc) ->
    io:fwrite("~p ~p ~n", [Name, Head]),
    build_worker(Name, Tail, Acc).

%%====================================================================

build_name(Name, {A, B, C, D}) ->
    list_to_atom("worker_" ++
		     Name ++ "_" ++
		     integer_to_list(A) ++ "_" ++
		     integer_to_list(B) ++ "_" ++
		     integer_to_list(C) ++ "_" ++
		     integer_to_list(D));

build_name(Name, {A, B, C, D, E, F}) ->
    list_to_atom("worker_" ++
		     Name ++ "_" ++
		     integer_to_list(A) ++ "_" ++
		     integer_to_list(B) ++ "_" ++
		     integer_to_list(C) ++ "_" ++
		     integer_to_list(D) ++ "_" ++
		     integer_to_list(E) ++ "_" ++
		     integer_to_list(F));

build_name(Name, {A, B, C, D, E, F, G, H}) ->
    list_to_atom("worker_" ++
		     Name ++ "_" ++
		     integer_to_list(A) ++ "_" ++
		     integer_to_list(B) ++ "_" ++
		     integer_to_list(C) ++ "_" ++
		     integer_to_list(D) ++ "_" ++
		     integer_to_list(E) ++ "_" ++
		     integer_to_list(F) ++ "_" ++
		     integer_to_list(G) ++ "_" ++
		     integer_to_list(H)).
