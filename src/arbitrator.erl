-module(arbitrator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(COOL_BEFORE_SET, 5000).
-define(EXTERNAL_TEST_HOST, "www.google.com").
-define(EXTERNAL_TEST_PORT, 80).
-define(NODE_NAME, "master").

-export([start_link/0,
	 interface_name_and_ip/2,
	 found_target_domain/2,
	 found_wrong_domain/2]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

interface_name_and_ip("lo", _Ip) ->
    ok;
interface_name_and_ip("docker" ++ _Rest, _Ip) ->
    ok;

interface_name_and_ip("br-" ++ _Rest, _Ip) ->
    ok;

interface_name_and_ip(Name, Ip) ->
    %% will only accept interfaces that can reach the outside
    case gen_tcp:connect(?EXTERNAL_TEST_HOST, ?EXTERNAL_TEST_PORT, [{ip, Ip}]) of
	{ok, _S} ->
	    gen_server:call(?MODULE, {interface, Name, Ip});
	_ ->
	    {ok, skipped}
    end.

found_target_domain(Ip, OurInterfaceAddr) ->
    gen_server:cast(?MODULE, {found_target_domain, Ip, OurInterfaceAddr}).

found_wrong_domain(Ip, OurInterfaceAddr) ->
    gen_server:cast(?MODULE, {found_wrong_domain, Ip, OurInterfaceAddr}).

%%====================================================================
%% Behaviour
%%====================================================================

init(_) ->
    {ok, #{interfaces => #{}}}.

handle_call({interface, Name, Ip}, _From, State=#{interfaces := Interfaces}) ->
    io:fwrite("Name interface ~p ~p~n", [Name, Ip]),
    {reply, ok, State#{interfaces := Interfaces#{Name => Ip}}};

handle_call(What, _From, State) ->
    {reply, {ok, What, State}, State}.

handle_cast({found_target_domain, Ip, OurInterfaceAddr}, State0) ->
    State = handle_target_domain(Ip, OurInterfaceAddr, State0),
    {noreply, State, ?COOL_BEFORE_SET};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(timeout, State=#{ip := Ip, our := Our}) ->
    io:fwrite("JUDGE: ~p ~p~n", [Our, Ip]),
    set_name(Our),
    {noreply, State};

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_target_domain(_Ip, {127,0,0,1}, State) ->
    State;

handle_target_domain(Ip, OurInterfaceAddr, _State) ->
    #{ip => Ip, our => OurInterfaceAddr}.

set_name({A, B, C, D}) ->
    Node = list_to_atom(?NODE_NAME ++ "@" ++
			    integer_to_list(A) ++ "." ++
			    integer_to_list(B) ++ "." ++
			    integer_to_list(C) ++ "." ++
			    integer_to_list(D)),
    net_kernel:start([Node, longnames]).
