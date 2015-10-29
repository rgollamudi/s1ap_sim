%% @author ramanagollamudi
%% @doc @todo Add description to enb_s1.


-module(enb_s1).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4]).
-compile([export_all, debug_info]).


-include("s1ap.hrl").
-include("ueSim.hrl").




%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {sock,
				state = ?S1NOTSETUP,		%% options are notSetup amd setup after S1 response received
				enbTblRef,
				ueTblRef,
				fragment = <<>>
			   }).


start_link({IP, Tproto, Port}, EnbName,  TblRef, UeTblRef) ->
	gen_server:start_link(?MODULE, [IP, Tproto, Port, EnbName, TblRef, UeTblRef], []).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([PeerIp, Tproto, PeerPort, EnbName, EnbTblRef, UeTblRef]) ->
	
	%%EnbParams = ets:lookup_element(EnbTblRef, EnbName, 4),
	
	Options = [binary, {active, true}],
	case Tproto of
		tcp ->
			case gen_tcp:connect(PeerIp, PeerPort, Options) of
				{ok, Sock} -> 
					ets:update_element(EnbTblRef, EnbName, 
									   [{2, self()}, {3, Sock}, {4, fun(M) -> gen_tcp:send(Sock, M) end}]),
					ets:insert(EnbTblRef, {self(), ?S1NOTSETUP}),
					{ok, #state{sock = Sock, 
								  enbTblRef = EnbTblRef, ueTblRef = UeTblRef, state = s1Notsetup}};
				{error, Reason} ->
					io:format("Failed to connect ot ~s~n", Reason),
					{error, Reason}
			end;
		sctp ->
			case gen_sctp:connect(PeerIp, PeerPort, Options) of
				{ok, Sock} -> 
					ets:update_element(EnbTblRef, EnbName,
									    [{2, self()}, {3, Sock}, {4, fun(M) -> gen_sctp:send(Sock, M) end}]),
					ets:insert(EnbTblRef, {self(), ?S1NOTSETUP}),
					{ok, #state{sock = Sock, 
								  enbTblRef = EnbTblRef, ueTblRef = UeTblRef, state = s1Notsetup}};
				{error, Reason} ->
					io:format("Failed to connect ot ~s~n", Reason),
					{error, Reason}
			end;
		usrsctp ->
			case gen_sctp:connect(PeerIp, PeerPort, Options) of
				{ok, Sock} -> 
					ets:update_element(EnbTblRef, EnbName,
									    [{2, self()}, {3, Sock}, {4, fun(M) -> gen_sctp:send(Sock, M) end}]),
					ets:insert(EnbTblRef, {self(), ?S1NOTSETUP}),
					{ok, #state{sock = Sock, 
								  enbTblRef = EnbTblRef, ueTblRef = UeTblRef, state = s1Notsetup}};
				{error, Reason} ->
					io:format("Failed to connect ot ~s~n", Reason),
					{error, Reason}
			end
	end.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({s1msg, S1Msg}, State) ->
	%%io:format("got s1 msg to send ~w on sock ~n", [S1Msg]),
	case gen_tcp:send(State#state.sock, S1Msg) of
		ok -> {noreply, State};
		{error, Reason} ->
			io:format("S1 Msg send failed ~w~n", [Reason]),
			error(Reason)
	end,
	{noreply, State};

handle_cast({reconnect, PeerIp, Port}, State) ->
	Options = [binary, {active, true}, {ip, PeerIp}],
	case gen_tcp:connect(PeerIp, Port, Options) of
		{ok, Sock} -> 
			%%io:format("enbS1 got connect to mme sock= ~w~n", [Sock]),
			{noreply, State#state{sock = Sock}};
		{error, Reason} ->
			io:format("Failed to connect ot ~s~n", Reason),
			{error, Reason}
	end;


handle_cast({enb_ready, _UeS1apId, UeSimPid}, State) ->
	%%io:format("handling enb ready State = ~w, ue Pid ~w~n", [State, UeSimPid]),
	case ets:lookup(State#state.enbTblRef, self()) of
		[{_, ?S1NOTSETUP}] -> gen_fsm:send_event(UeSimPid, ?S1NOTSETUP);
		[{_, ?S1SETUP}] -> gen_fsm:send_event(UeSimPid, enb_ready);
		X -> io:format("enb_s1 ~w : did not find S1setup rec in enbTbl ~w~n", [self(), X])
	end,
	%%case State#state.state of
	%%	?S1NOTSETUP -> gen_fsm:send_event(UeSimPid, ?S1NOTSETUP);
	%%	?S1SETUP -> gen_fsm:send_event(UeSimPid, enb_ready)
	%%end,
	{noreply, State};

handle_cast({send_uplink_nas, NasPdu}, State) ->
	{noreply, State};

handle_cast(Request, State) ->
	io:format("got Unknown request  ~w~n", [Request]),
	{noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({tcp, _Socket, Data}, State) ->
	%%io:format("got data from Socket~w~n", [Data]),
	Msg = list_to_bitstring([State#state.fragment, Data]),
	Fragment = rcv_pdu(Msg, State),
	{noreply, State#state{fragment = Fragment}};
		
handle_info({tcp_closed, Socket}, State) ->
	io:format("got tcp_closed from Socket ~w~n", [Socket]),
	{noreply, State};


handle_info(Info, State) ->
	io:format("got unknown data into handle_info ~w ~w~n", [Info, State]),
	{noreply, State}.




%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


process_msg({_, {_MsgType, ?'id-S1Setup', _Criticality, {_S1MsgRecType, IeList} }} = S1msg, State) ->
	%%logger:log_msg("Got S1 Msg ~w~n ~w~n", [State, S1msg]),
	ets:update_element(State#state.enbTblRef, self(), {2, ?S1SETUP}),
	{noreply, State#state{state = ?S1SETUP}};


process_msg({_, {_MsgType, ?'id-downlinkNASTransport', _Criticality, {_S1MsgRecType, IeList} }} = S1msg, State) ->
	%%logger:log_msg("Enb_s1: Got downlink NAS Transport Msg ~w~n ~w~n", [State, S1msg]),
	case lists:keyfind(?'id-eNB-UE-S1AP-ID', 2, IeList) of
		{_, _, _, EnbUeId} ->  
			case ets:lookup(State#state.ueTblRef, EnbUeId) of
				[{_, UeSimPid, _, _}] -> 
					case lists:keyfind(?'id-MME-UE-S1AP-ID', 2, IeList) of
						{_, _, _, MmeUeId} -> 
							%%logger:log_msg("Got downlink NAS Transport for UeS1ap ~w ~w~n", [EnbUeId, MmeUeId]),
							ets:update_element(State#state.ueTblRef, EnbUeId, {3, MmeUeId});
						_ -> logger:log_msg("strange - no MmeUeS1apId found in} Msg ~w~n", [S1msg])
					end,
					gen_fsm:send_event(UeSimPid, {?'id-downlinkNASTransport', S1msg});
				[] -> logger:log_msg("strange - no ueSimPid for ~w~n", [EnbUeId])
			end;
		false -> logger:log_msg("DownlinkNASTransport msg with no EnbUeS1apId rcvd~w~n", [S1msg])
	end,
	{noreply, State};




process_msg({_, {_MsgType, ?'id-InitialContextSetup', _Criticality, {_S1MsgRecType, IeList} }} = S1msg, State) ->
%%logger:log_msg("Enb_s1: Got downlink InitialContextSetup ~w~n ~w~n", [State, S1msg]),
	case lists:keyfind(?'id-eNB-UE-S1AP-ID', 2, IeList) of
		{_, _, _, EnbUeId} ->  
			case ets:lookup(State#state.ueTblRef, EnbUeId) of
				[{_, UeSimPid, _, _}] -> 
					case lists:keyfind(?'id-MME-UE-S1AP-ID', 2, IeList) of
						{_, _, _, MmeUeId} -> 
							%%logger:log_msg("EnbS1: Got InitialConextSetup for UeS1ap ~w ~w~n", [EnbUeId, MmeUeId]),
							ets:update_element(State#state.ueTblRef, EnbUeId, {3, MmeUeId});
						_ -> logger:log_msg("strange - no MmeUeS1apId found in} Msg ~w~n", [S1msg])
					end,
					gen_fsm:send_event(UeSimPid, {?'id-InitialContextSetup', S1msg});
				[] -> logger:log_msg("strange - no ueSimPid for ~w~n", [EnbUeId])
			end;
		false -> logger:log_msg("Initial Context Setup msg with no EnbUeS1apId rcvd~w~n", [S1msg])
	end,
	{noreply, State};
	



process_msg({_, {_MsgType, ?'id-UEContextRelease', _Criticality, {_S1MsgRecType, IeList} }} = S1msg, State) ->
	%%logger:log_msg("Enb_s1: Got UE Context Release Msg ~w~n ~w~n", [State, S1msg]),
	case lists:keyfind(?'id-eNB-UE-S1AP-ID', 2, IeList) of
		{_, _, _, EnbUeId} ->  
			case ets:lookup(State#state.ueTblRef, EnbUeId) of
				[{_, UeSimPid, _, _}] -> 
					gen_fsm:send_event(UeSimPid, {?'id-UEContextRelease', S1msg});
				[] -> logger:log_msg("strange - no ueSimPid for ~w~n", [EnbUeId])
			end;
		false -> logger:log_msg("Ue Context Release msg with no EnbUeS1apId rcvd~w~n", [S1msg])
	end,
	{noreply, State};


process_msg({_, {_MsgType, _, _Criticality, {_S1MsgRecType, IeList} }} = S1msg, State) ->
	%%logger:log_msg("Enb_s1:  State ~w~n  Enb S1 Rcvd Msg : ~w~n", [State, S1msg]),
	case lists:keyfind(?'id-eNB-UE-S1AP-ID', 2, IeList) of
		{_, _, _, EnbUeId} ->  
			case ets:lookup(State#state.ueTblRef, EnbUeId) of
				[{_, UeSimPid, _, _}] -> 
					gen_fsm:send_event(UeSimPid, S1msg);
				[] -> logger:log_msg("strange - no ueSimPid for ~w~n", [EnbUeId])
			end;
		false -> logger:log_msg("Ue Context Release msg with no EnbUeS1apId rcvd~w~n", [S1msg])
	end,
	{noreply, State};

process_msg(Msg, State) ->
	logger:log_msg("Got msg ~w~n", [Msg]),
	{noreply, State}.




rcv_pdu(<<>>, State) -> <<>>;
rcv_pdu(Pdu, State) ->
	case s1ap:decode('S1AP-PDU', Pdu) of
		{ok, S1msg, Rest} -> 
				process_msg(S1msg, State),
				rcv_pdu(Rest, State);
		{error, Reason} ->
				%%io:format("Unable to decode S1AP-PDU for ~w~n", [Reason]),
				Pdu;
		X ->
			%%io:format("Enb_S1 Process: ~w, s1ap:decode returns unknown error ~w~n", [self(), X]),
			Pdu
	end.
