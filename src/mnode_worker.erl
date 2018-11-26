-module(mnode_worker).
-behaviour(gen_server).
-define(TARGET_DOMAIN, "_erlang._tcp").

-include_lib("kernel/src/inet_dns.hrl").

-export([start_link/2]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%=============================================================================
%% API
%%=============================================================================

start_link(Name, Addr) ->
    gen_server:start_link(?MODULE, {Name, Addr}, []).

%%====================================================================
%% Behaviour
%%====================================================================

%% It we start the worker with a IPv4 address,
%% init and continue at handle_info(timeout).
%% It not and IPv4 address, do nothing.

init({Name, Addr={_A, _B, _C, _D}}) ->
    {ok, #{name => Name, addr => Addr, socket => missing}, 0};

init({Name, _Opts}) ->
    {ok, #{name => Name, addr => missing, socket => missing}}.

%% This module does not support any calls
%%

handle_call(What, _From, State) ->
    {reply, {ok, What, State}, State}.

%% This module does not support any casts
%%

handle_cast(_What, State) ->
    {noreply, State}.

%% There are two important info patterns
%%  A) timeout that is triggered from init above
%%  B) udp packages

handle_info(timeout, State=#{name := Name, addr := Addr}) ->
    arbitrator:interface_name_and_ip(Name, Addr),
    {ok, Socket} = initial_work(Addr),
    {noreply, State#{socket := Socket}};

handle_info({udp, _Socket, Ip, _Port, Packet}, State=#{addr := Addr}) ->
    handle_packet(Ip, Packet, Addr),
    {noreply, State};

handle_info(_What, State) ->
    {noreply, State}.

%% Close socket if open when terminating
%%

terminate(_Reason, #{sock := missing}) ->
    ok;
terminate(_Reason, #{sock := Sock}) ->
    gen_udp:close(Sock),
    ok.

%% Classic code_change when nothing needs to
%% to be done

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

socket_options(InterfaceAddr) ->
    MulticastAddr = {224,0,0,251},
    [{multicast_if, InterfaceAddr},
     {reuseaddr, true},
     {multicast_ttl,4},
     {active, true},
     {mode, binary},
     {ip, MulticastAddr},
     {add_membership,{MulticastAddr, InterfaceAddr}}].

initial_work(InterfaceAddr) ->
    ListenPort = 5353,
    gen_udp:open(ListenPort,
		 socket_options(InterfaceAddr)).

%%====================================================================

handle_packet(Ip, Packet, OurInterfaceAddr) ->
    handle_decoded_packet(Ip, OurInterfaceAddr, inet_dns:decode(Packet)).

%%====================================================================

handle_decoded_packet(_Ip, _OurInterfaceAddr, {error, Reason}) ->
    io:fwrite("ERROR ~p~n", [Reason]);

handle_decoded_packet(Ip, OurInterfaceAddr, {ok, #dns_rec{header = _Header,
							  qdlist = [#dns_query{domain = Domain}]}}) ->
    handle_domain(Ip, OurInterfaceAddr, string:str(Domain, ?TARGET_DOMAIN));

handle_decoded_packet(_Ip, _OurInterfaceAddr, {ok, #dns_rec{header = _Header}}) ->
    ok.

%% Update arbirator about zero conf messages
%%

handle_domain(Ip, OurInterfaceAddr, 0) ->
    arbitrator:found_wrong_domain(Ip, OurInterfaceAddr);

handle_domain(Ip, OurInterfaceAddr, _N) ->
    arbitrator:found_target_domain(Ip, OurInterfaceAddr).
