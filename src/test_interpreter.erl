%% @author rgollamudi
%% @doc @todo Add description to test_interpreter.


-module(test_interpreter).
-behaviour(gen_server).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2, exec_test/1, exec_prg/1, exec_test/2, exec_prg/2]).



-include("s1ap.hrl").
-include("ueSim.hrl").


-define(EPS_MOBILE_ID_IEI,		16#17).
-define(IMSI_TYPE_IEI,				2#001).
-define(UE_NETWORK_CAPABILITY_IEI,  2#00000000).		
-define(ESM_MESSAGE_CONTAINER_IEI,  2#00000000).
-define(ESM_INFO_TRF_FLAG_IEI, 		16#D).
-define(APN_IEI,					28).
-define(PCO_IEI,					27).
-define(DEVICE_PROPERTIES_IEI,			16#C).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {enbTblRef,
				ueTblRef,
				enbSup,
				ueSup,
				enbInfo = #enbInfo{tai = [{plmnId, <<16#123456:24>>},  {tac, <<16#10:16>>}], 
								   cgi = [ {plmnId, <<16#123456:24>>}, {cellId, <<123456:28>>}],
								   rrcEstCause = 'mt-Access'},
				enbUeS1apId = 1,
				conf}).

-define(SERVER, ?MODULE).

start_link(EnbSupPid, UeSupPid) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [EnbSupPid, UeSupPid], []).


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
init([EnbSupPid, UeSupPid]) ->
	EnbTblRef = ets:new(enbTblRef, [public, 
									{write_concurrency, true}, {read_concurrency, true}] ),
	UeTblRef = ets:new(ueTblRef, [public, 
									{write_concurrency, true}, {read_concurrency, true}] ),
	
	
		
	register(ueSim_sup, UeSupPid),
	register(enbSim_sup, EnbSupPid),

    {ok, #state{enbTblRef = EnbTblRef, ueTblRef = UeTblRef,
				enbSup = EnbSupPid, ueSup = UeSupPid}}.


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
handle_call({exec, Cmd}, _From, State) ->
	io:format("~n got exec msg ~w~n", [Cmd]),
	 {reply, ok, exec_cmd(Cmd, State)};


handle_call({exec_set, PrgFile}, _From, State) ->
	io:format("~n got exec set ~w~n", [PrgFile]),
	Conf = case file:consult(PrgFile) of
		{ok, C} -> C;
		Err -> io:format("Unable to read config File ~w ~n", [Err]), error(Err)
	end,
	%%config_reader:start_link(Config),
	Instructions = lookup(instructset, Conf),
%%	io:format("~nInstruction ~w~n", [Instructions]),
	{reply, ok, lists:foldl(fun exec_cmd/2, State#state{conf = Conf}, Instructions)}.




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
handle_cast(exit, State) ->
    {stop, shutdown, State}.


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
handle_info(_Info, State) ->
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
terminate(_Reason, State) ->
	%%Child = supervisor:which_children(State#state.enbSup),
	%%lists:foreach(fun ({Id, _, _, _}) -> 
	%%					   supervisor:terminate_child(State#state.enbSup, Id) end, Child),
	exit(State#state.enbSup, kill),
	ets:delete(State#state.enbTblRef),
    ok.



%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




%% ====================================================================
%% Internal functions
%% ====================================================================
exec_test(Cmd) ->
	gen_server:call(?SERVER, {exec, Cmd}).
exec_test(Pid, Cmd) ->
	gen_server:call(Pid, {exec, Cmd}).

exec_prg(PrgFile) ->
	gen_server:call(?SERVER, {exec_set, PrgFile}).

exec_prg(Pid, PrgFile) ->
	gen_server:call(Pid, {exec_set, PrgFile}).

exec_cmd({startAenb, {IP, Tproto, Port}, EnbName} = A, State) ->
	EnbSup = case State#state.enbSup of
				 undefined ->  
					Pid =  case enb_s1_sup:start_link() of
						 {ok, P} -> P;
						 Err -> io:format("Enb S1 Sup failed to start ~w~n", [Err]),
								exit(Err)
					 end;
				 _ -> 
					 io:format("enb s1 sup already started ~w~n", [State]),
					 State#state.enbSup
			 end,
	case enb_s1_start({IP, Tproto, Port}, EnbName, EnbSup, State#state.enbTblRef, State#state.ueTblRef) of
		{ok, _} -> State#state{enbSup = EnbSup};
		_ ->
			io:format("Could not start enb S1~w~n", [EnbName]),
			State#state{enbSup = EnbSup}
	end,
	io:format("Done Starting Enb_S1~n"), timer:sleep(1),
	State#state{enbSup = EnbSup};

exec_cmd({startEnbs, NumEnb, {IP, Tproto, Port}, EnbName, Conf, Incr} = C, State) ->
	EnbSup = case State#state.enbSup of
				 undefined -> 
					 %%io:format("starting enb s1 sup ~w~n", [State]),
					 case enb_s1_sup:start_link() of
						 {ok, Pid} -> Pid;
						 Err -> io:format("Enb S1 Sup failed to start ~w~n", [Err]),
								exit(Err)
					 end;
				 _ -> 
					 %%io:format("enb s1 sup already started ~w~n", [State]),
					 State#state.enbSup
			 end,
	AutoIncr = case Incr of
					{_, true} -> true;
					_ -> false
				end,
				
	startEnbs(NumEnb, {IP, Tproto, Port}, EnbName, Conf, AutoIncr, State#state{enbSup = EnbSup});



exec_cmd({startUes, NumUes, ConnectTo, UeParams, AutoIncrement} = _C, State) ->
	%%io:format("in exec cmd startUes ~w~n", [_C]),timer:sleep(1),
	UeSup = case State#state.ueSup of
				 undefined -> 
					 io:format("starting Ue Sim sup ~n"),
					 case ue_sim_sup:start_link() of
						 {ok, Pid} -> Pid;
						 Err -> io:format("Ue Sim  Sup failed to start ~w~n", [Err]),
								exit(Err)
					 end;
				 _ -> 
					 %%io:format("Ue Sup s1 sup already started ~w~n", [State]),
					 State#state.ueSup
			 end,
	startUes(NumUes, ConnectTo, UeParams, AutoIncrement, State#state{ueSup = UeSup});





exec_cmd({s1SetupReq, Enbs, SetupParams} = _R,  State)  ->
	%%io:format("in exec cmd s1Req Cmd = ~w~n, State = ~w~n", [_R, State]),
	case ets:lookup(State#state.enbTblRef, Enbs) of
		[{_, S1Pid, _, _}] ->
			s1_hlpr:send_s1_setup(SetupParams, S1Pid), 
			State;
		[] ->
			io:format("Not S1 set up for ~w~n", [Enbs]),
			State
	end;
	

exec_cmd({initialUEMessage, Enbs, MsgParm} = _M, State) ->
	%%io:format("~nin exec cmd initialUEMessage ~w~n", [_M]),
	%%io:format("State = ~w~n", [State]),
	ets:insert(State#state.ueTblRef, {s1ap_ie_val(enbUeS1apId, MsgParm), {enbUeS1apId, s1ap_ie_val(enbUeS1apId, MsgParm)}}),
	case ets:lookup(State#state.enbTblRef, Enbs) of
		[{_, S1Pid}] ->
			s1_hlpr:send_initialUEMessage(MsgParm, S1Pid), 
			State;
		[] ->
			io:format("Not S1 set up for ~w~n", [Enbs]),
			State
	end;

exec_cmd({wait, Time}, State) ->
	timer:sleep(Time), State;
	
exec_cmd(Cmd,  _State)  ->
	io:format("Command format wrong~w~n", [Cmd]).
	
	
find_val(Ie, List) ->
	case lists:keyfind(Ie, 1, List) of
		{_, P} -> P;
		false ->
			%%io:format("Ie not defined in conf file~w~n", [Ie]),
			[]
	end.

s1ap_ie_val(IeType, List) ->
	%%io:format("IeType to encode = ~w~n", [IeType]), timer:sleep(10),
	case IeType of
		glbEnbId ->
			#'Global-ENB-ID'{pLMNidentity =  find_val(plmnId, List), 'eNB-ID' = find_val(enbId, List)};

		supportedTA ->
			[#'SupportedTAs-Item'{tAC = find_val(tac, List), broadcastPLMNs = find_val(bdPlmnId, List)}];

		nasPdu ->
			bld_nas_pdu(find_val(nasPdu, List));

		tai -> 
			#'TAI'{pLMNidentity = find_val(plmnId, find_val(tai, List)), 
				   	tAC = find_val(tac, find_val(tai, List))};
		
		cgi ->
			#'EUTRAN-CGI'{pLMNidentity = find_val(plmnId, find_val(cgi, List)), 
				   			'cell-ID' = find_val(cellId, find_val(cgi, List))};
		_ ->
			find_val(IeType, List)
	end.

send_s1_setup(R,  EnbPid)  ->
	%%io:format("S1 Setup ~w~n", [R]), timer:sleep(10),
	{ok, S1Msg} = s1codec:encodeS1Msg(initiatingMessage, 'S1SetupRequest', ?'id-S1Setup', reject,
							 			[{?'id-Global-ENB-ID', reject, mandatory, s1ap_ie_val(glbEnbId, R)}, 
											{?'id-eNBname', ignore, optional, s1ap_ie_val(enbName, R)}, 
											{?'id-SupportedTAs', reject, mandatory, s1ap_ie_val(supportedTA, R)},
											{?'id-DefaultPagingDRX', ignore, mandatory, s1ap_ie_val(pagingDrx, R)},
											{?'id-CSG-IdList', reject, optional, s1ap_ie_val(csgIdList, R)}]   ),
									 
	gen_server:cast(EnbPid, {s1msg, S1Msg}).
	

send_initialUEMessage(R, EnbPid) ->
	
	
	{ok, S1Msg} = s1codec:encodeS1Msg(initiatingMessage, 'initialUEMessage', ?'id-initialUEMessage', ignore,
									  [{?'id-eNB-UE-S1AP-ID', reject, mandatory, s1ap_ie_val(enbUeS1apId, R)},
										{?'id-NAS-PDU', reject, mandatory, s1ap_ie_val(nasPdu, R)},
		    							{?'id-TAI', reject, mandatory,  s1ap_ie_val(tai, R)},
		   								{?'id-EUTRAN-CGI', ignore, mandatory, s1ap_ie_val(cgi, R)},
		   								{?'id-RRC-Establishment-Cause', ignore, mandatory, s1ap_ie_val(rrcEstCause, R)},
		   								{?'id-S-TMSI', reject, optional, s1ap_ie_val(stmsi, R)},
		   								{?'id-CSG-Id', reject, optional, s1ap_ie_val(csgId, R)},
		   								{?'id-GUMMEI-ID', reject,  optional, s1ap_ie_val(gummeiId, R)},
		   								{?'id-CellAccessMode', reject, optional, s1ap_ie_val(cellAccessMode, R)},
		   								{?'id-GW-TransportLayerAddress', ignore, optional, s1ap_ie_val(gwTlAddr, R)},
		   								{?'id-RelayNode-Indicator', reject, optional, s1ap_ie_val(relayNodeInd, R)},
		   								{?'id-GUMMEIType', ignore, optional, s1ap_ie_val(gummeiType, R)},
		   								{?'id-Tunnel-Information-for-BBF', ignore, optional, s1ap_ie_val(tunInfoBbf, R)},
		   								{?'id-SIPTO-L-GW-TransportLayerAddress', ignore, optional, s1ap_ie_val(siptoTlAddr, R)},
		   								{?'id-LHN-ID', ignore, optional, s1ap_ie_val(lhnId, R)}
		   							   ]
									 ),
	io:format("~nSending initial UE msg ~w~n", [S1Msg]),
	gen_server:cast(EnbPid, {s1msg, S1Msg}).


	
startEnbs(0, _, _, _, _, State) -> State;
startEnbs(NumEnb, {IP, Tproto, Port}, EnbName, Conf, AutoIncr, State) ->
	EnbS1Name = string:concat(EnbName, integer_to_list(NumEnb)),
%%	io:format("EnbS1Name ~w, EnbName ~w~n", [EnbS1Name, EnbName]),
%%	ets:insert(State#state.enbTblRef, {EnbName, 1, 2, State#state.enbInfo}),
	case enb_s1_start({IP, Tproto, Port}, EnbS1Name, State#state.enbSup, State#state.enbTblRef, State#state.ueTblRef) of
		{ok, Pid} -> 
			send_s1_setup(Conf ++ [{enbName, EnbS1Name}], Pid),
			Config = case AutoIncr of
				true ->
					Conf2 = case lists:keyfind(enbId, 1, Conf) of
						{_,  {'macroENB-ID', MacroEnbId}} ->
							EnbIncrVal = case lists:keyfind(enbIdIncrBy, 1, Conf) of
								{_, IVal} -> IVal;
								_ -> 0
							end,
							NewMacroId = bits_to_num(MacroEnbId) + EnbIncrVal,
							lists:keyreplace(enbId, 1, Conf,  {enbId, {'macroENB-ID', <<NewMacroId:20>>}});
						_ -> 
							io:format("Strange, bad config. No macroEnb-ID defined in config ~w~n", [Conf]),
							Conf
					end,
					Conf3 = case lists:keyfind(tac, 1, Conf2) of
						{_,  TacVal} ->
							TacIncrVal = case lists:keyfind(tacIncrBy, 1, Conf2) of
								{_, IV} ->
									case (bits_to_num(TacVal) rem IV) of
										0 ->  1;
										_ -> 0
									end;
								_ -> 0
							end,
							NewTac = bits_to_num(TacVal) + TacIncrVal,
							%%io:format("NewTac Val ~w~n", [NewTac]),
							lists:keyreplace(tac, 1, Conf2,  {tac, <<NewTac:16>>});
						_ -> 
							io:format("Strange, bad config. No TAC defined in config ~w~n", [Conf2]),
							Conf2
					end,
					Conf3;
				_ ->
					Conf
			end,
			%%io:format("here 6~n"),
			startEnbs(NumEnb - 1, {IP, Tproto, Port}, EnbName, Config, AutoIncr, State);
		_ ->
			io:format("enb s1 start failed"),
			State			%% exec_cmd needs to return state
	end.


enb_s1_start({IP, Tproto, Port}, EnbName, EnbSupRef, EnbTblRef, UeTblRef) ->
	io:format("Test Interpreter: EnbName ~w~n", [EnbName]),
	ets:insert(EnbTblRef, {EnbName, 1, 2, 3}),
%%	ets:lookup(EnbTblRef, EnbName),
	EnbSpec = {EnbName,{enb_s1, start_link, [{IP, Tproto, Port}, EnbName, EnbTblRef, UeTblRef]},
	      permanent,2000,worker,[enb_s1]},
	case supervisor:start_child(EnbSupRef, EnbSpec) of 
		{ok, Child} -> 
			%%io:format("started child ~w, Pid: ~w~n",[EnbName, Child]), 
			{ok, Child}; 
		{ok, Child, _} -> 
			%%io:format("started child2~n"),  
			{ok, Child}; 
		{error, Reason} -> 
			io:format("unable to start Enb ~w~n", [Reason]), 
			{error, Reason}
	end.



startUes(0, ConnectTo, UeParams, AutoIncrement, State) -> State;
startUes(NumEnb, ConnectTo, UeParams, AutoIncrement, State) ->
%%	io:format("in startingUes ~w, ~w~n", [NumEnb, UeParams]), %%timer:sleep(100),
	UeS1apId = State#state.enbUeS1apId + 1,
	case supervisor:start_child(State#state.ueSup, [#ueParams{connect_to = ConnectTo,
															  ueS1apId = UeS1apId,
															  ueTblRef = State#state.ueTblRef,
															  enbTblRef = State#state.enbTblRef,
															  ueConfig = UeParams}]) of
		{error, Reason} -> 
			io:format("unable to start UE ~w~n", [Reason]); 

		{ok, Child} -> 
			%%io:format("started ~w, ~w~n", [NumEnb, Child]),
			ok;

		{ok, Child, _} -> 
			%%io:format("started ~w, ~w~n", [NumEnb, Child]), 
			ok

	end,
	 UeParam2 = case AutoIncrement of 
					{_, true} -> 
						case lists:keyfind(mobileId, 1, UeParams) of
							{_,  {imsi, ImsiVal}} ->
								ImsiIncrVal = case lists:keyfind(mobileIdIncrBy, 1, UeParams) of
									{_, IVal} -> IVal;
									_ -> 0
								end,
								NewImsiVal = incr_list_num(ImsiVal, ImsiIncrVal),
								lists:keyreplace(mobileId, 1, UeParams,  {mobileId, {imsi, NewImsiVal}});
						_ -> 
							io:format("Strange, bad config. No MobileId defined in config ~w~n", [UeParams]),
							UeParams
						end;
					_ -> UeParams
				end,
	%%io:format("UeParams2 ~w~n_", [UeParam2]),
	startUes(NumEnb-1, ConnectTo, UeParam2,  AutoIncrement, State#state{enbUeS1apId = UeS1apId}).



lookup(Tag, Conf) ->
	 case lists:keyfind(Tag, 1, Conf) of 
				{Tag, Value} -> Value;
				false -> {error, noinstance}
	end.


bld_nas_pdu(Params) ->
	list_to_bitstring(lists:foldr(fun nasie/2, [], Params)).
	%%io:format("msg ~w~n", [Msg]),

bld_esm_msg(E) ->
	list_to_bitstring(lists:foldr(fun nasie/2, [], E)).

nasie({IeType, V}, Acc) ->
	[ie(IeType, V) | Acc].

ie(IeType, Value) ->
	case IeType of
		esmMsg ->
			bld_esm_msg(Value);
		mobileId ->
			[{_, MobileIdType} | Rest] = Value,
			case MobileIdType of
				imsi ->
					[{_, Imsi} | _] = Rest,
					NumDigits = lists:foldl(fun (_, Acc) -> Acc + 1 end, 0, Imsi),
					[Digit1 | ImsiRest] = Imsi,
					[<<?EPS_MOBILE_ID_IEI:8>>, <<(NumDigits div 2):8>>, <<Digit1:4>>, 
					   <<(if (NumDigits rem 2) =:= 0 -> 0;
									true -> 1 end):1>>, <<?IMSI_TYPE_IEI:3>>,
					   nibble_swap(ImsiRest, [])
					];
				guti -> ok;
				emei -> ok
			end;
		ueNetCapability ->
			[<<?UE_NETWORK_CAPABILITY_IEI:1>>, <<(byte_size(Value)):8>>, Value];
		esmInfoTrfFlag ->
			[<<?ESM_INFO_TRF_FLAG_IEI:4>>, <<Value:4>>];
		apn ->
			[<<?APN_IEI:8>>, <<(lists:foldl(fun (_, Acc) -> Acc + 1 end, 0, Value)):8>>, Value];
		pco ->
			[<<?PCO_IEI:8>>, <<(lists:foldl(fun (_, Acc) -> Acc + 1 end, 0, Value)):8>>, Value];
		devProp ->
			[<<?DEVICE_PROPERTIES_IEI:3>>, <<Value:4>>];
		_ -> Value
	end.


nibble_swap([], N) -> N;
nibble_swap([E|[]], N) ->  nibble_swap([], N ++ [<<2#1111:4>>] ++ [<<E:4>>]);
nibble_swap([E1 | [E2 | R]], N) ->  nibble_swap(R, N ++ [<<E2:4>>] ++ [<<E1:4>>]).


bits_to_num(N) ->
	S = bit_size(N),
	L = 8 - (S rem 8),
	X = list_to_bitstring([<<0:L>>, N]),
	binary:decode_unsigned(X).


incr_list_num(List, Incr) ->
  {Res, _} = 
	lists:foldr(fun (A, {ResList, IncrVal}) -> 
				   {[(case (A + IncrVal) >= 10 of
						 true -> (A + IncrVal) rem 10;
						 _ -> A + IncrVal
					 end) | ResList], (case (A + IncrVal) >= 10 of
										   true -> (A + IncrVal) div 10;
										   _ -> 0
									   end)} end, {[], Incr}, List),
  Res.