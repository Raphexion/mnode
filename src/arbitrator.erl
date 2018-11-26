-module(arbitrator).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(COOL_BEFORE_SET, 5000).
-define(EXTERNAL_TEST_HOST, "www.google.com").
-define(EXTERNAL_TEST_PORT, 80).
-define(NODE_NAME, "master").
-define(COOKIE, mysecretecookie).

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

interface_name_and_ip(Name, Ip) ->
    gen_server:cast(?MODULE, {interface, Name, Ip}).

found_target_domain(Ip, OurInterfaceAddr) ->
    gen_server:cast(?MODULE, {found_target_domain, Ip, OurInterfaceAddr}).

found_wrong_domain(Ip, OurInterfaceAddr) ->
    gen_server:cast(?MODULE, {found_wrong_domain, Ip, OurInterfaceAddr}).

%%====================================================================
%% Behaviour
%%====================================================================

init(_) ->
    {ok, #{banned => #{}}}.

handle_call(What, _From, State) ->
    {reply, {ok, What, State}, State}.

handle_cast({interface, Name, Ip}, State0) ->
    State = interface_logic_high(Name, Ip, State0),
    {noreply, State, ?COOL_BEFORE_SET};

handle_cast({found_target_domain, Ip, OurInterfaceAddr}, State0) ->
    State = handle_target_domain(Ip, OurInterfaceAddr, State0),
    {noreply, State, ?COOL_BEFORE_SET};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    io:fwrite("time to judge~n"),
    judge(State),
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

%% Key function where we change our name and set the cookie
%%

set_name({A, B, C, D}) ->
    Node = list_to_atom(?NODE_NAME ++ "@" ++
			    integer_to_list(A) ++ "." ++
			    integer_to_list(B) ++ "." ++
			    integer_to_list(C) ++ "." ++
			    integer_to_list(D)),
    net_kernel:start([Node, longnames]),
    erlang:set_cookie(node(), ?COOKIE).

%% handle target domain helps analyse what we should do
%% in a particular situation
%%

handle_target_domain(_Ip, {127,0,0,1}, State) ->
    State;

handle_target_domain(Ip, Our, State=#{banned := Banned}) ->
    target_domain_logic(Ip, Our, State, maps:find(Our, Banned)).

%% If the interface is banned (i.e, in the map of banned above)
%% then do nothing, else save both the ip and interface
%%

target_domain_logic(Ip, Our, State, {ok, true}) ->
    io:fwrite("tdl 1: ~p ~p ~n", [Ip, Our]),
    State;

target_domain_logic(Ip, Our, State, error) ->
    io:fwrite("tdl 2: ~p ~p ~n", [Ip, Our]),
    State#{ip => Ip, our => Our}.

judge(#{high_prio := Ip, our := Ip}) ->
    set_name(Ip);

judge(#{low_prio := Ip, our := Ip}) ->
    set_name(Ip);

judge(#{high_prio := Ip}) ->
    set_name(Ip);

judge(State) ->
    io:fwrite("error: unable to judge ~p~n", [State]).

%% Either ban the IP or try to connect to external device
%%

interface_logic_high("lo", Ip, State=#{banned := Banned}) ->
    State#{banned => Banned#{Ip => true}};

interface_logic_high("docker" ++ _Rest, Ip, State=#{banned := Banned}) ->
    State#{banned => Banned#{Ip => true}};

interface_logic_high("br-" ++ _Rest, Ip, State=#{banned := Banned}) ->
    State#{banned => Banned#{Ip => true}};

interface_logic_high("virbr" ++ _Rest, Ip, State=#{banned := Banned}) ->
    State#{banned => Banned#{Ip => true}};

interface_logic_high("tun" ++ _Rest, Ip, State=#{banned := Banned}) ->
    State#{banned => Banned#{Ip => true}};

interface_logic_high(_Name, Ip={127,0,0,1}, State=#{banned := Banned}) ->
    State#{banned => Banned#{Ip => true}};

interface_logic_high(Name, Ip, State) ->
    io:fwrite("good candidate ~p~n", [Name]),
    ExternalConnectRes = gen_tcp:connect(?EXTERNAL_TEST_HOST, ?EXTERNAL_TEST_PORT, [{ip, Ip}], 1000),
    interface_logic(Name, Ip, ExternalConnectRes, State).

%% Help logic to decided if the interface has high or low priority
%%

interface_logic(Name, Ip, {ok, _S}, State) ->
    io:fwrite("good candidate ~p high~n", [Name]),
    State#{high_prio => Ip};

interface_logic(Name, Ip, {error, timeout}, State) ->
    io:fwrite("good candidate ~p high~n", [Name]),
    State#{low_prio => Ip}.
